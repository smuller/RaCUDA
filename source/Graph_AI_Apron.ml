(* Quentin Carbonneaux - 2016-2017 *)

(* Interface with the APRON library to compute
   logical invariants at all program points.
   We base that on Bertrand Jeannet's fixpoint
   library (in lib/).
*)

open Types
open Graph_Types

module Translate = struct

  (* The logical conditions (type logic) and program expressions
     (type expr) need to be turned into what Apron understands.

     One difficulty is that abstract interpretation can only
     manipulate conjunctive states, so we use the DNF module
     to normalize all disjunctions at the top of logic
     expressions.
  *)

  module E = Apron.Texpr1
  module C = Apron.Tcons1

  (* A disjunction of an array of conjuncted constraints,
     or true (if None). *)
  type disj = C.earray list option

  let texpr_of_poly p: E.expr =
    let open Polynom in
    let rec pow n x =
      if n = 0 then failwith "invalid argument" else
      if n = 1 then x else
      E.Binop (E.Mul, pow (n-1) x, x, E.Int, E.Rnd)
    in
    let trf = function
      | Factor.Var v -> E.Var (Apron.Var.of_string v)
      | _ -> failwith "invalid argument"
    in
    let trm m =
      let (f, e) = Monom.pick m in
      Monom.fold (fun f e trm ->
        E.Binop (E.Mul, pow e (trf f), trm, E.Int, E.Rnd))
        m (pow e (trf f))
    in
    match
      Poly.fold begin fun m k trp ->
        let k = E.Cst (Apron.Coeff.s_of_float k) in
        let mk =
          if Monom.is_one m then k else
          E.Binop (E.Mul, k, trm m, E.Int, E.Rnd)
        in
        match trp with
        | None -> Some mk
        | Some e -> Some (E.Binop (E.Add, mk, e, E.Int, E.Rnd))
      end p None
    with
    | None -> E.Cst (Apron.Coeff.s_of_int 0)
    | Some e -> e

  let texpr_of_expr e: E.expr =
    let rec tr = function
      | ERandom -> E.Cst (Apron.Coeff.i_of_float neg_infinity infinity)
      | EVar id -> E.Var (Apron.Var.of_string id)
      | ENum n -> E.Cst (Apron.Coeff.s_of_int n)
      | ESub (ENum 0, e) -> E.Unop (E.Neg, tr e, E.Int, E.Rnd)
      | EAdd (e1, e2) -> E.Binop (E.Add, tr e1, tr e2, E.Int, E.Rnd)
      | ESub (e1, e2) -> E.Binop (E.Sub, tr e1, tr e2, E.Int, E.Rnd)
      | EMul (e1, e2) -> E.Binop (E.Mul, tr e1, tr e2, E.Int, E.Rnd)
    in tr e

  let disj_of_logic env l: disj =
    let cmp e1 c e2 =
      let e1, c, e2 =
        match c with
        | Le -> e2, C.SUPEQ, e1
        | Lt -> e2, C.SUP, e1
        | Ge -> e1, C.SUPEQ, e2
        | Gt -> e1, C.SUP, e2
        | Eq -> e1, C.EQ, e2
        | Ne -> e1, C.DISEQ, e2
      in
      let e1 = texpr_of_expr e1 in
      let e2 = texpr_of_expr e2 in
      DNF.lift (C.make
        (E.of_expr env (E.Binop (E.Sub, e1, e2, E.Int, E.Rnd))) c)
    in
    let formula = DNF.of_logic cmp l in
    if DNF.is_true formula then None else
    Some (List.map begin fun cnj ->
      let ea = C.array_make env (List.length cnj) in
      List.iteri (C.array_set ea) cnj;
      ea
    end formula)

end

type func_info =
  { fi_env: Apron.Environment.t
  ; fi_vars: Apron.Var.t array
  ; fi_args: Apron.Var.t array
  ; fi_rets: Apron.Var.t array
  ; fi_arg_tmps: Apron.Var.t array
  ; fi_ret_tmps: Apron.Var.t array
  ; fi_func: func
  }

type transfer =
  | TGuard of Translate.disj
  | TAssign of Apron.Var.t * Apron.Texpr1.t
  | TCall of func_info * func_info * Apron.Var.t array * Apron.Var.t array
  | TReturn of func_info * func_info * Apron.Var.t array * Apron.Var.t array

module HyperGraph = struct

  (* The fixpoint library needs to represent programs
     using hypergraphs.  We are close enough with
     our Graph representation, but tweaks are needed
     for function call/returns.
  *)

  type info = (string, func_info) Hashtbl.t

  type vertex = id * int  (* pair of function name and node id *)
  type hedge = int

  let print_vertex info fmt (vf, vp) =
    let f = (Hashtbl.find info vf).fi_func in
    let pos = f.fun_body.g_position.(vp) in
    Utils.print_position fmt pos

  let vara_of_idl l =
    Array.of_list (List.map Apron.Var.of_string l)

  let from_funcl fl =
    let new_edge =
      let next_edge = ref (-1) in
      fun () -> incr next_edge; !next_edge in

    (* vertex:   type of vertices
       hedge:    type of hyper-edges
       unit:     data associated to the vertices
       transfer: data associated to the hyper-edges
       info:     data associated to the whole graph
    *)
    let info = Hashtbl.create 51 in
    let g: (vertex, hedge, unit, transfer, info) PSHGraph.t =
      PSHGraph.create PSHGraph.stdcompare 3 info in

    (* Add all the vertices and fill the info table first. *)
    List.iter begin fun f ->
      let tmpl s = List.map ((^) s) in
      let vars =
        f.fun_args @ f.fun_rets @
        f.fun_vars @ tmpl "arg." f.fun_args in
      let env = Apron.Environment.make (vara_of_idl vars) [||] in
      Hashtbl.add info f.fun_name
        { fi_env = env
        ; fi_vars = vara_of_idl f.fun_vars
        ; fi_args = vara_of_idl f.fun_args
        ; fi_rets = vara_of_idl f.fun_rets
        ; fi_arg_tmps = vara_of_idl (tmpl "arg." f.fun_args)
        ; fi_ret_tmps = vara_of_idl (tmpl "ret." f.fun_rets)
        ; fi_func = f
        };
      for node = 0 to Array.length f.fun_body.g_edges - 1 do
        PSHGraph.add_vertex g (f.fun_name, node) ();
      done;
    end fl;

    (* Then add all the edges.
       It is trivial except for the ACall case.
    *)
    List.iter begin fun f ->
      let {fi_env=env;_} as fi = Hashtbl.find info f.fun_name in
      Array.iteri begin fun src el ->
        List.iter begin fun (act, dst) ->
          let src = f.fun_name, src in
          let dst = f.fun_name, dst in
          match act with
          | AWeaken | ANone ->
            PSHGraph.add_hedge g (new_edge ())
              (TGuard None) ~pred:[|src|] ~succ:[|dst|];
          | AGuard log ->
            let disj = Translate.disj_of_logic env log in
            PSHGraph.add_hedge g (new_edge ())
              (TGuard disj) ~pred:[|src|] ~succ:[|dst|];
          | AAssign (id, e) ->
            let te = Translate.texpr_of_expr e in
            let te = Apron.Texpr1.of_expr env te in
            let v = Apron.Var.of_string id in
            PSHGraph.add_hedge g (new_edge ())
              (TAssign (v, te)) ~pred:[|src|] ~succ:[|dst|];
          | ACall (idl, idf', el) ->
            let f'i = Hashtbl.find info idf' in
            let f'start = f'i.fi_func.fun_body.g_start in
            let f'end = f'i.fi_func.fun_body.g_end in
            let vl = vara_of_idl idl in
            let vl' = List.map begin function
              | EVar v -> Apron.Var.of_string v
              | _ -> Utils._TODO "expression argument in call"
              end el in
            let vl' = Array.of_list vl' in
            PSHGraph.add_hedge g (new_edge ())
              (TCall (fi, f'i, vl, vl'))
              ~pred:[|src|] ~succ:[|idf', f'start|];
            PSHGraph.add_hedge g (new_edge ())
              (TReturn (fi, f'i, vl, vl'))
              ~pred:[|src; idf', f'end|] ~succ:[|dst|];
        end el;
      end f.fun_body.g_edges;
    end fl;
    g

end

module Solver = struct

  (* Here we use the fixpoint library to find an
     abstract state for each vertex of the hypergraph.
  *)

  module A = Apron.Abstract1

  let make_fpmanager man graph abstract_init apply =
    let info = PSHGraph.info graph in
    let dont_print _ _ = () in
    { Fixpoint.bottom = begin fun (vf, _) ->
        A.bottom man (Hashtbl.find info vf).fi_env
      end
    ; canonical = begin fun _ _ -> () end
    ; is_bottom = begin fun _ -> A.is_bottom man end
    ; is_leq = begin fun _ -> A.is_leq man end
    ; join = begin fun _ -> A.join man end
    ; join_list = begin fun _ absl ->
        A.join_array man (Array.of_list absl)
      end
    ; odiff = None
    ; widening = begin fun _ -> A.widening man end
    ; abstract_init = abstract_init
    ; arc_init = begin fun _ -> () end
    ; apply = apply man graph
    ; print_vertex = HyperGraph.print_vertex info
    ; print_hedge = Format.pp_print_int
    ; print_abstract = A.print
    ; print_arc = begin fun fmt () ->
        Format.pp_print_string fmt "()"
      end
    ; accumulate = false
    ; print_fmt = Format.std_formatter
    ; print_analysis = false
    ; print_component = false
    ; print_step = false
    ; print_state = false
    ; print_postpre = false
    ; print_workingsets = false
    ; dot_fmt = None
    ; dot_vertex = dont_print
    ; dot_hedge = dont_print
    ; dot_attrvertex = dont_print
    ; dot_attrhedge = dont_print
    }

  (* We can now define the action of transfers on
     the abstract state.
  *)

  let linexpr_array abs =
    let env = A.env abs in
    Array.map begin fun var ->
      let e = Apron.Linexpr1.make ~sparse:true env in
      Apron.Linexpr1.set_coeff e var (Apron.Coeff.s_of_int 1);
      e
    end

  let apply_TGuard man abs disj =
    let abs_and_disj =
      match disj with
      | None -> [abs]
      | Some disj ->
        List.map (A.meet_tcons_array man abs) disj
    in
    match abs_and_disj with
    | [] -> A.bottom man (A.env abs)
    | [x] -> x
    | disj -> A.join_array man (Array.of_list disj)

  let apply_TAssign man abs v e =
    A.assign_texpr man abs v e None

  let apply_TCall man abs _ fi' _ arga =
    (* 1. remove all non-argument variables *)
    let penv = Apron.Environment.make arga [||] in
    let abs = A.change_environment man abs penv false in
    (* 2. rename parameters into f' temporary arguments *)
    A.rename_array_with man abs arga fi'.fi_arg_tmps;
    (* 3. embed in f' environment *)
    A.change_environment_with man abs fi'.fi_env false;
    (* 4. assign f' arguments from the temporaries *)
    if fi'.fi_arg_tmps <> [||] then
    A.assign_linexpr_array_with man abs
      fi'.fi_args
      (linexpr_array abs fi'.fi_arg_tmps) None;
    abs

  let apply_TReturn man abs abs' fi fi' reta arga =
    (* abs is from the caller, abs' is from the callee *)
    (* 1. leave only argument temporaries and return vars *)
    let vara = Array.append (fi'.fi_rets) (fi'.fi_arg_tmps) in
    let penv = Apron.Environment.make vara [||] in
    let res = A.change_environment man abs' penv false in
    (* 2. rename return vars into return temporaries *)
    A.rename_array_with man res fi'.fi_rets fi'.fi_ret_tmps;
    (* 3. rename argument temporaries arguments into arguments *)
    A.rename_array_with man res fi'.fi_arg_tmps arga;
    (* 4. embed into the caller environment *)
    A.unify_with man res abs;
    (* 5. assign actual return variables *)
    if fi'.fi_ret_tmps <> [||] then
    A.assign_linexpr_array_with man res
      reta
      (linexpr_array res fi'.fi_ret_tmps) None;
    (* 6. get rid of return temporaries *)
    A.change_environment_with man res fi.fi_env false;
    res

  let apply man graph hedge tabs =
    let transfer = PSHGraph.attrhedge graph hedge in
    let res =
      match transfer with
      | TGuard disj -> apply_TGuard man tabs.(0) disj
      | TAssign (v, e) -> apply_TAssign man tabs.(0) v e
      | TCall (fi, fi', reta, arga) ->
        apply_TCall man tabs.(0) fi fi' reta arga
      | TReturn (fi, fi', reta, arga) ->
        apply_TReturn man tabs.(0) tabs.(1) fi fi' reta arga
    in ((), res)

  let compute man graph fstart =
    let info = PSHGraph.info graph in
    let fs = (Hashtbl.find info fstart).fi_func in
    let starts =
      PSette.singleton
        PSHGraph.stdcompare.PSHGraph.comparev
        (fstart, fs.fun_body.g_start) in
    let absinit (vf, _) =
      A.top man (Hashtbl.find info vf).fi_env in
    let fpman = make_fpmanager man graph absinit apply in
    fpman,
    Fixpoint.analysis_std
      fpman graph starts
      (Fixpoint.make_strategy_default
        ~vertex_dummy:("", -1)
        ~hedge_dummy:(-1)
        graph starts)

end

let debug_print fmt info graph res =
  let print_vara =
    Print.array
      ~first:"(@[" ~sep:",@ " ~last:"@])"
      Apron.Var.print in
  let print_transfer fmt = function
    | TGuard (Some disj) ->
      Format.fprintf fmt "Guard %a"
        (Print.list ~first:"(@[<h>" ~sep:" ||@ " ~last:"@])"
          (Apron.Tcons1.array_print
            ~first:"@[<h>" ~sep:" &&@ " ~last:"@]"))
        disj;
    | TGuard None ->
      Format.fprintf fmt "Guard True";
    | TAssign (v, e) ->
      Format.fprintf fmt "Assign %a = %a"
        Apron.Var.print v
        Apron.Texpr1.print e;
    | TCall (_, f', reta, arga) ->
      Format.fprintf fmt "Call %a = %s%a"
        print_vara reta f'.fi_func.fun_name print_vara arga;
    | TReturn (_, f', reta, arga) ->
      Format.fprintf fmt "Return %a = %s%a"
        print_vara reta f'.fi_func.fun_name print_vara arga;
  in
  let print_hedge_attr fmt hedge transfer =
    Format.fprintf fmt "%a"
      print_transfer transfer
  in
  PSHGraph.print_dot
    begin fun fmt (vf, vn) -> Format.fprintf fmt "%s_%d" vf vn end
    begin fun fmt hid -> Format.fprintf fmt "e_%d" hid end
    begin fun fmt v _ ->
      Solver.A.print fmt (PSHGraph.attrvertex res v)
    end
    print_hedge_attr
    fmt graph

(* Common API for abstract interpretation modules. *)

type base = Oct.t
type absval = (base Apron.Manager.t * base Solver.A.t)

let analyze ~dump fl fstart =
  let graph = HyperGraph.from_funcl fl in
  let info = PSHGraph.info graph in
  let man = Oct.manager_alloc () in
  let (fpman, res) = Solver.compute man graph fstart in

  if dump then begin
    let (fn, oc) = Filename.open_temp_file "" ".dot" in
    let fpng = fn ^ ".png" in
    let fmt = Format.formatter_of_out_channel oc in
    debug_print fmt info graph res;
    close_out oc;
    let cmd = Printf.sprintf "dot -O -Tpng %s" fn in
    if (try Sys.command cmd with Sys_error _ -> 1) <> 0 then
      Printf.eprintf
        "Error: '%s' failed, be sure to have Graphviz installed.\n"
        cmd
    else Utils.show_remove_png fpng;
    Sys.remove fn;
  end;

  let resh = Hashtbl.create 51 in
  Hashtbl.iter begin fun fname {fi_env; fi_func=f; _} ->
    let top = Apron.Abstract1.top man fi_env in
    Hashtbl.add resh fname
      (Array.make (Array.length f.fun_body.g_edges) (man, top));
  end info;

  PSHGraph.iter_vertex res
  begin fun (vf, vn) abs ~pred ~succ ->
    let map = Hashtbl.find resh vf in
    map.(vn) <- (man, abs);
  end;
  resh

let is_nonneg (man, abs) pol =
  let texpr = Translate.texpr_of_poly pol in
  let env = Solver.A.env abs in
  let texpr = Translate.E.of_expr env texpr in
  let tcons = Translate.C.make texpr Translate.C.SUPEQ in
  Solver.A.sat_tcons man abs tcons

let get_nonneg _abs = []
