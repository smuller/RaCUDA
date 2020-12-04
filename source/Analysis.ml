(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Graph
open Types
open Polynom
open Focus
open Graph_Types

module S = Set.Make(String)

type analysis =
  | Size_change
  | Water_mark

type stats =
  { mutable num_lpvars: int
  ; mutable num_lpcons: int
  ; mutable max_focus: int
  ; lp_runtime: float ref
  }

let stats =
  { num_lpvars = 0
  ; num_lpcons = 0
  ; max_focus = 0
  ; lp_runtime = ref 0.
  }

let reset_stats () =
  begin
    stats.num_lpvars <- 0;
    stats.num_lpcons <- 0;
    stats.max_focus <- 0;
    stats.lp_runtime := 0.;
  end

type order =
  | Ge
  | Le
  | Eq

let swap_order = function
  | Ge -> Le
  | Le -> Ge
  | Eq -> Eq

let classify_monom globals m =
  let has_glo = ref false in
  let has_loc = ref false in
  let _ =
    Monom.var_exists (fun v ->
      if IdSet.mem v globals then
        has_glo := true
      else
        has_loc := true;
      false (* keep iterating *)
    ) m
  in
  match !has_glo, !has_loc with
  | true, false -> `Global
  | false, true -> `Local
  | true, true -> `Mixed
  | false, false -> `Constant

module Potential
: sig
  type annot
  type focus_annot
  type solution
  val new_annot: Monom.t list -> annot
  val dump: Format.formatter -> annot -> unit
  val of_poly: Poly.t -> annot
  val monoms: annot -> Monom.t list
  val exec_assignment: (id * expr * bool) -> annot -> annot
  val constrain: annot -> order -> annot -> unit
  val weaken: Poly.t list -> annot -> (annot * focus_annot list)
  val split: IdSet.t -> annot -> ((* local *) annot * (* global *) annot)
  val merge: (annot * annot) -> annot
  val addmul: annot -> int -> annot -> annot
  val solve_min: Poly.t list -> annot -> solution option
  val annot_sol: solution -> annot -> Poly.t
  val focus_annot_sol: solution -> focus_annot -> (float * float)
  val remove_constant: annot -> annot
  val remove_var: annot -> id -> annot
  val k_annot: float -> annot -> annot
  val create_annot_prob: float -> float -> annot -> annot -> annot (* probabilistic branching*)
end
= struct

  (* This is the core of the analysis.
     We define what potential annotations are,
     and how they are:

       1. changed by assignments,
       2. constrained between each other (=, or >=),
       3. changed by probabilistic statements.

  *)

  (* LP variables are simple integers. *)
  type lpvar = int

  (* To generate few LP constraints, our potential
     does not map monomials to LP variables directly
     but to linear combinations of those.

     A linear combination (LC) is represented as a hash
     table mapping LP variables to their coefficient
     in the linear sum. LCs represent the coefficients of monomials
     in annotations as functions over LP variables.
     e.g., P = (kv_0*v_0 + kv_1 * v_1) * m_1 + (kv'_1 * v_1 + kv_2 * v_2) * m_2 + (kv_3 * v_3).z
     lc1      : v_0 --> kv_0, v_1 --> kv_1
     lc2      : v_1 --> kv'_1, v_2 --> kv_2
     lc3      : v_3 --> kv_3
     annot    : m_1 --> lc1, m_2 --> lc2, z --> lc3
  *)
  module M = Map.Make(Monom)
  type linexpr = (lpvar, float) Hashtbl.t
  type annot = linexpr M.t
  type focus_annot = lpvar * lpvar
  type solution = float array

  let new_lpvar () =
    stats.num_lpvars <- stats.num_lpvars + 1;
    Clp.add_column
      { Clp.column_obj = 0.
      ; Clp.column_lower = neg_infinity
      ; Clp.column_upper = infinity
      ; Clp.column_elements = [| |]
      };
    Clp.number_columns () - 1

  let new_linexpr () =
    Hashtbl.create 5

  (* returns a new annot v_1 * m_1 + ... + v_n * m_n *)
  let new_annot monoms =
    List.fold_left begin fun annot m ->
      if M.mem m annot then annot else
      let le = new_linexpr () in
      let v = new_lpvar () in
      Hashtbl.add le v 1.;
      M.add m le annot
    end M.empty monoms

  let dump fmt a =
    let dump_le fmt le =
      Format.fprintf fmt "@[<h>";
      let first = ref true in
      Hashtbl.iter begin fun lpv k ->
        Format.fprintf fmt
          (if !first
           then "%g v%d"
           else "@ +@ %g v%d")
          k lpv;
        first := false;
      end le;
      Format.fprintf fmt "@]";
    in
    Format.fprintf fmt "@[<v>";
    M.iter begin fun m le ->
      Format.fprintf fmt "Monom (%a) %a@,"
        (Monom.print ~ascii:true) m
        dump_le le
    end a;
    Format.fprintf fmt "@]"

  (* Performs: le = k*le
     le : v --> kv then le : v --> k*kv
   *)
  let linexpr_mul k le =
    Hashtbl.iter
      begin
        fun v kv -> Hashtbl.replace le v (k *. kv)
      end le

  (* Performs: le2 += k * le1
     le1 : v --> kv1, le2: v -->kv2, then le2: v --> kv2 + k*kv1
  *)
  let linexpr_addmul le2 k le1 =
    Hashtbl.iter begin fun v kv1 ->
      let kv2 = try Hashtbl.find le2 v with Not_found -> 0. in
      Hashtbl.replace le2 v (kv2 +. k *. kv1)
    end le1

  (* Performs: add monoms in the given poly and linear combination to a given annotation
     add_poly: linexpr -> (Poly -> annot -> annot)
     e.g.,
     le = (v_1 --> k_1)
     p = (x^2 --> k)
     a = (x^2 --> le1), le1 = (v_2 --> k_2)
     Then add_poly le p a = (x^2 --> le1'), le1' = (v_1 --> k*k_1, v_2 --> k_2)
  *)
  let add_poly le =
    Poly.fold
      begin fun monom k new_annot ->
        let le_old =
          try M.find monom new_annot
          with Not_found -> new_linexpr ()
        in
        linexpr_addmul le_old k le;
        M.add monom le_old new_annot
      end

  let remove_var annot id =
    M.filter
      ( fun m _ ->
        not (Monom.var_exists ((=) id) m)
      )
      annot

  (* add an assignment v <-- e into annot
     e is a poly
  *)
  let exec_assignment (v, e, is_pot) annot =
    (*let annot = if is_pot then annot else remove_var annot v
    in *)
    if is_pot then
      let subst e =
        M.fold begin fun monom le ->
          add_poly le (monom_subst v e monom)
          end annot M.empty
      in
      match e with
      | ERandom ->
         let fresh = Utils.fresh_name () in
         let e = Poly.of_monom (Monom.of_var fresh) 1. in
         subst e
      | e -> subst (Poly.of_expr e)
    else annot

  (* add a constraint sum(kv_i * v_i) o k *)
  let add_lprow_array ?(k=0.) arr o =
    begin
      stats.num_lpcons <- stats.num_lpcons + 1;
      Clp.add_row
        { Clp.row_lower = if o = Le then neg_infinity else k
        ; Clp.row_upper = if o = Ge then infinity else k
        ; Clp.row_elements = arr
        };
    end

  (* Constrain the linear expression le
     against the constant k.
  *)
  let add_lprow ?(k=0.) le o =
    let l =
      Hashtbl.fold begin fun v kv l ->
        if abs_float kv <= fsmall
          then l
          else (v, kv) :: l
      end le []
    in
    if l <> [] then begin
      if false then
      begin
        List.iter (fun (v, kv) ->
          Format.eprintf "%g * v%d + " kv v;) l;
        Format.eprintf "0 %s %g@."
          (match o with Ge -> ">=" | Le -> "<=" | Eq -> "=")
          k;
      end;
      add_lprow_array (Array.of_list l) o ~k
    end

  (* return annot sum(v_i * m_i) where v_i = k_i *)
  let of_poly p =
    Poly.fold begin fun monom k annot ->
      let v = new_lpvar () in
      let le = new_linexpr () in
      Hashtbl.add le v 1.;
      add_lprow le Eq ~k;
      M.add monom le annot
    end p M.empty

  (* return all monomials in a as a list *)
  let monoms a =
    M.fold (fun m _ l -> m :: l) a []

  (* return annot without constant monomial *)
  let remove_constant annot =
    M.filter
      ( fun m le ->
        if not (Monom.is_one m) then true else false
      )
      annot


  (* Point-wise constrain annot1 o annot2.
     m is in both annot1 and annot2, le1 - k*le2 o 0.
     m is in annot1, but not in annot2, le1 o 0.
     m is in annot2, but not in annot1, 0 o le2
  *)
  let constrain annot1 o annot2 =
    begin
      (* Constrain common coefficients. *)
      M.iter begin fun m le1 ->
        let le = Hashtbl.copy le1 in
        begin
          try linexpr_addmul le (-1.) (M.find m annot2)
          with Not_found -> ()
        end;
        add_lprow le o
      end annot1;
      (* Constrain the coefficients in annot2 only. *)
      M.iter begin fun m le2 ->
        if not (M.mem m annot1) then
          add_lprow le2 (swap_order o);
      end annot2;
    end

  (* return an annot and a list of LP variables *)
  let expand l pl =
    let le = new_linexpr () in
    (* Compute Σ(kᵢ * plᵢ) as an annotation. *)
    let plsum, kpl =
      List.fold_left begin fun (plsum, kpl) p ->
        let kp = new_lpvar () in
        Hashtbl.clear le;
        Hashtbl.add le kp 1.;
        (add_poly le p plsum, kp :: kpl)
      end (M.empty, []) pl in
    (* Add plsum and l (in plsum). *)
    let plsum =
      M.merge begin fun _ leo vo ->
        let le =
          match leo with
          | Some le -> le
          | None -> new_linexpr ()
        in
        match vo with
        | Some v -> Hashtbl.add le v 1.; Some le
        | None -> failwith "impossible"
      end plsum l in
    (plsum, kpl)

  (* Create a new annotation using the init function to
     create new coefficients.  The returned annotation
     has an LP variable for every monomial in the list
     of polynomials pl and in the annotation annot.
  *)
  let frame_from ?init:(init=fun _ -> new_lpvar ()) pl annot =
    let frame = M.mapi (fun m _ -> init m) annot in
    let frame =
      List.fold_left begin fun frame p ->
        Poly.fold begin fun m _ frame ->
          if M.mem m frame
            then frame
            else M.add m (init m) frame
        end p frame
      end frame pl in
    frame

  (* Returns annot' such that  annot' >= annot, using a
     list of non-negative polynomials.  If the polynomials
     are actually null, the returned potential has the
     same value as the passed one.
  *)
  let weaken pl annot =
    let l = frame_from pl annot in
    let exannot, kpl = expand l pl in
    constrain exannot Eq annot;
    let annot', kpl' = expand l pl in
    List.iter2 begin fun kp1 kp2 ->
      assert (kp1 <> kp2);
      add_lprow_array [| kp2, 1.; kp1, -1. |] Ge;
    end kpl kpl';
    (annot', List.rev (List.combine kpl kpl'))

  (* Split an annotation into two annotations that add
     to the argument annotation.  The first element of
     the pair result is a purely local annotation and
     the second one is purely global.
  *)
  let split gs annot =
    M.fold begin fun m le (purel, pureg) ->
      match classify_monom gs m with
      | `Mixed ->
        (* Mixed term, we currently zero it out.
           FIXME, we should handle cases where the
           monom is a product of a pure global and
           a pure local part.
        *)
        add_lprow le Eq;
        (purel, pureg)
      | `Local -> (M.add m le purel, pureg)
      | `Global | `Constant -> (purel, M.add m le pureg)
    end annot (M.empty, M.empty)

  (* An operation opposite to the split one. *)
  let merge (purel, pureg) =
    M.merge begin fun m leo1 leo2 ->
      match leo1, leo2 with
      | Some le, None
      | None, Some le -> Some le
      | Some le1, Some le2 ->
        if not (Monom.is_one m) then
          let _ =
            Format.eprintf "Merge error on monom: %a@."
              (Monom.print ~ascii:true) m in
          failwith "invalid Potential.merge call"
        else
          let le = new_linexpr () in
          linexpr_addmul le 1. le1; (* ----------------------------- odd... *)
          linexpr_addmul le 1. le2;
          Some le
      | None, None -> assert false
    end purel pureg

  let addmul annot1 k annot2 =
    M.merge begin fun _ leo1 leo2 ->
      match leo1, leo2 with
      | Some le, None
      | None, Some le -> Some le
      | Some le1, Some le2 ->
        let le = new_linexpr () in
        linexpr_addmul le 1. le1;
        linexpr_addmul le (float_of_int k) le2;
        Some le
      | None, None -> assert false
    end annot1 annot2

  (* Perform k * annot *)
  let k_annot k annot =
    M.map
      begin
        fun le -> linexpr_mul k le; le
      end annot

  let create_annot_prob p1 p2 annot1 annot2 =
    let p1_annot1 = k_annot p1 annot1 in
    let p2_annot2 = k_annot p2 annot2 in
    addmul p1_annot1 1 p2_annot2

  let solve_min pl start_annot =
    let absl = ref [] in
    M.iter begin fun m le ->
      (* Zero out all fresh variables created by
         non-deterministic assignments.
      *)
      if Monom.var_exists Utils.is_fresh m then
        add_lprow le Eq
    end start_annot;
    let l = frame_from pl start_annot ~init:begin fun _ ->
        let v = new_lpvar () and abs = new_lpvar () in
        add_lprow_array [| abs, 1.; v, +1. |] Ge;
        add_lprow_array [| abs, 1.; v, -1. |] Ge;
        absl := abs :: !absl;
        v
      end in
    let exannot, kpl = expand l pl in
    constrain exannot Eq start_annot;
    let vmax = new_lpvar () in
    List.iter (fun k ->
      add_lprow_array [| vmax, 1.; k, -1. |] Ge) kpl;

    Time.wrap_duration stats.lp_runtime begin fun () ->
      (* Initial solving call trying to minimize the
         coefficients of the frame L and the max of
         all coefficients of pl polynomials.
      *)
      let obj = Clp.objective_coefficients () in
      List.iter (fun k -> obj.(k) <- 1.) !absl;
      Clp.change_objective_coefficients obj;
      Clp.set_log_level 0;
      Clp.initial_solve ();
      if Clp.status () <> 0 then None else

      (* Second solving call, this time minimizing
         the sum of coefficients of the polynomials
         in pl.
      *)
      let () = if false then
        Printf.printf "[i] Second optimization round.\n" in
      let sol = Clp.primal_column_solution () in
      add_lprow_array [| vmax, 1. |] Le ~k:sol.(vmax);
      List.iter (fun k ->
        obj.(k) <- 0.;
        add_lprow_array [| k, 1. |] Eq ~k:sol.(k)) !absl;
      List.iter (fun k -> obj.(k) <- 1.) kpl;
      Clp.change_objective_coefficients obj;
      Clp.primal ();
      if Clp.status () <> 0 then None else

      (* Build polynomial solution. *)
      let sol = Clp.primal_column_solution () in
      Some sol
    end

  let annot_sol sol a =
    M.fold begin fun m le poly ->
      let k =
        Hashtbl.fold begin fun v kv k ->
          k +. kv *. sol.(v)
        end le 0.
      in Poly.add_monom m k poly
    end a (Poly.zero ())

  let focus_annot_sol sol (ka, kb) =
    (sol.(ka), sol.(kb))

end

(* rewrite the annotation *)
let run ai_results ai_is_bot ai_is_nonneg (gl, fl) start degree analysis_type query tick_var enable_neededness dump_neededness =
  Clp.reset ();
  reset_stats ();

  (* The collection of annotations for each function. *)
  let annots = Hashtbl.create 11 in
  let new_func_annot fn =
    let last_id =
      try fst (Hashtbl.find annots fn)
      with Not_found -> -1 in
    let id = last_id + 1 in
    let func_annot = ref [||] in
    Hashtbl.add annots fn (id, func_annot);
    (id, func_annot)
  in

  let gs = IdSet.of_list gl in
  let debug_dumps = ref [] in
  let pzero = Potential.of_poly (Poly.zero ()) in

  (* Find all the non-negative focus functions at a
     given program point.
  *)
  let find_focus f node focus =
    let ai = Hashtbl.find ai_results f in
    let ok f =
      List.for_all (ai_is_nonneg ai.(node)) f.checks in
    let res = List.filter ok focus in
    if false then begin
      let fn = List.find (fun g -> g.fun_name = f) fl in
      let fprint fmt {checks = c; proves = f; _} =
        Format.fprintf fmt (if c <> [] then "* %a" else ". %a")
          Poly.print f in
        Format.eprintf "At %a, using:@.%a@."
        Utils.print_position fn.fun_body.g_position.(node)
        (Print.list ~first:"@[<v>" ~sep:"@ " ~last:"@]" fprint) res
    end;
    stats.max_focus <- max (List.length res) stats.max_focus;
    res
  in
  let polys_of_focus = List.map (fun f -> f.proves) in

  (* Analyze the function with name fname for the query
     query.  The return value is a pair consisting of
     an identifier for the given run (used to create
     hints for function calls), and a final annotation
     for the start point of the function.
  *)
  let rec do_fun wmp ctx fname degree query =

    if degree = 0 then (-1, query) (* Optimization. *) else
    try
      let (id, mk_start_annot, end_annot) = List.assoc fname ctx in
      Potential.constrain end_annot Eq query;
      (id, mk_start_annot ())
    with Not_found ->

    let f = List.find (fun f -> f.fun_name = fname) fl in
    let body = f.fun_body in
    let focus = Focus.one :: f.fun_focus in

    (* For testing: print the result of needed analysis *)
    let neededness = Neededness.needed_variables f in
    if dump_neededness then 
      Graph.print_graph_f stdout (Some neededness) f;

    let monoms =
      let monoms = Potential.monoms query in
      List.fold_left begin fun monoms f ->
        Poly.fold (fun m _ ms -> m :: ms) f.proves monoms
      end monoms focus
    in

    let focus, monoms =
      List.filter (fun f -> Poly.degree f.proves <= degree) focus,
      List.filter (fun m -> Monom.degree m <= degree) monoms
    in

    let is_global m = 
      Monom.degree m < degree &&
      classify_monom gs m = `Global ||
      classify_monom gs m = `Constant
    in 
      

    (* TODO: precomputed global monomials *)
    let global_monoms = 
      List.filter is_global monoms
    in

    let weaken node a =
      let focus = find_focus fname node focus in
      let polys = polys_of_focus focus in
      let a, fa = Potential.weaken polys a in
      a, (fa, focus)
    in

    (* Constrain the potential annotation to be no less than the
       query when computing the water mark.
    *)
    let wm node a =
      match wmp with
      | None -> a
      | Some p ->
        Potential.constrain (fst (weaken node p)) Eq a;
        a
    in

    let rec_annot = ref None in (* in case of recursion *)

    let mk_rec_annot () =
      match !rec_annot with
      | Some annot -> annot
      | None ->
        let annot = Potential.new_annot monoms in
        rec_annot := Some annot;
        annot
    in

    (* Create the function annotation that will be returned. *)
    let (id, func_annot) = new_func_annot fname in
    let ctx = (fname, (id, mk_rec_annot, query)) :: ctx in

    let hints = Array.map (fun _ -> []) body.Graph.g_position in
    let annot = Array.map (fun _ -> `Todo) body.Graph.g_position in

    (* Create a new potential annotation resulting from executing one action (backwards) a', node --->act a *)
    let do_action node act a =
      wm node
        begin
          (* if ai_is_bot (Hashtbl.find ai_results fname).(node) then
                       (Printf.printf "bot\n";
                        a)
          else *)
          match act with
          | Graph.AWeaken ->
            let a, (fa, focus) = weaken node a in
            hints.(node) <- (List.combine fa focus);
            a
          | Graph.AGuard LRandom -> debug_dumps := a :: !debug_dumps; a
          | Graph.AGuard _ | Graph.AUnique _ | Graph.ANotUnique _ | Graph.AUnify
            | Graph.ANone | Graph.AProb _ | Graph.ACallUninterp _ -> debug_dumps := a :: !debug_dumps; a
          | Graph.ASwitch _ -> a
          | Graph.AAssign (v, e, ip) ->
             let _ = debug_dumps := a :: !debug_dumps in
             let a = Potential.exec_assignment (v, !e, !ip) a
             in
             (*Potential.dump Format.std_formatter a;
             Printf.printf "\n--------------\n"; *)
             debug_dumps := a :: !debug_dumps; a
          | Graph.AAddMemReads (v, v', ph, _, _, _)
            | Graph.AAddConflicts (v, v', ph, _, _) ->
             (match !ph with
              | None ->
                 ((* Printf.printf "WARNING: %s not filled in\n" v'; *)
                  a)
              (* failwith ("should have been filled in - " ^ v') *)
              | Some n ->
                 Potential.exec_assignment (v, EAdd (ENum n, EVar v), true) a)
          | Graph.ACall f' ->
            let (la, ga) = Potential.split gs a in
            let ga1 = Potential.new_annot global_monoms in
            let ga = Potential.addmul ga (-1) ga1 in
            let _, a = do_fun wmp ctx f' degree ga in
            let (_la', ga) = Potential.split gs a in
            let _, a1 = do_fun None [] f' (degree-1) ga1 in
            if wmp <> None then
              Potential.constrain (fst (weaken node pzero)) Eq a1;
            let (_la1, ga1) = Potential.split gs a1 in
            Potential.constrain _la' Eq pzero;
            Potential.constrain _la1 Eq pzero;
            let ga = Potential.addmul ga (+1) ga1 in
            let a = Potential.merge (la, ga) in
            a
        end
    in

    (* Annotate all program points starting from
       a given node. The potential at the end of
       the function is set to query.

       We follow a depth-first search on the graph
       and update annotations for nodes lazily, this
       allows to reduce the number of LP variables.

       It is assumed that all probabilistic branching
       statements have exactly 2 branches as in the
       second inner match of case

       In other cases, it requires computed annots from
       different paths the same
    *)
    let rec dfs node enable_neededness =
      match annot.(node) with
      | `Done a ->
        a
      | `Doing ->
        let monoms =
          if enable_neededness then
            (* Modifying the list of monoms *)
            let needed_vars = neededness node in
            let needed_vars = tick_var :: needed_vars in
            let needed_set = List.fold_left (fun m e -> S.add e m)
                               S.empty needed_vars in
            let rec is_needed monom =
              if Monom.is_one monom 
              then true
              else
                let helper factor i b =
                  match factor with
                    | Factor.Var x ->
                        if S.mem x needed_set then
                          b
                        else
                          false
                    | Factor.Max poly ->
                        Poly.fold (fun m c b -> is_needed m || b) poly false
                in
                Monom.fold helper monom true
            in
            List.filter is_needed monoms
          else monoms
        in
        (*let monoms = if ai_is_bot (Hashtbl.find ai_results fname).(node) then
                       (Printf.printf "bot\n";
                        [])
                     else monoms
        in *)
        let a = Potential.new_annot monoms in
        annot.(node) <- `Done a;
        a
      | `Todo ->
         (annot.(node) <- `Doing;
          let possible n =
            not (ai_is_bot (Hashtbl.find ai_results fname).(n))
          in
        let a =
          match body.Graph.g_edges.(node) with
          | [] ->
            if node <> body.Graph.g_end then Utils._TODO "It should not happen!";
            query
          | (act_1, node_1) :: (act_2, node_2) :: [] ->
            let annot_1 = do_action node act_1 (dfs node_1 enable_neededness) in
            let annot_2 = do_action node act_2 (dfs node_2 enable_neededness) in
            begin
              match (act_1, act_2) with
              | (Graph.AProb p_1, Graph.AProb p_2) ->
                let annot = Potential.create_annot_prob p_1 p_2 annot_1 annot_2 in
                annot
              (*| (Graph.ANotUnique lr, _) when !lr = LFalse -> annot_2
              | (_, Graph.ANotUnique lr) when !lr = LFalse -> annot_1 *)
              | _ ->
                 (match (possible node_1, possible node_2) with
                    (true, true) ->
                     Potential.constrain annot_1 Eq annot_2; annot_1
                  | (true, false) -> annot_1
                  | _ -> annot_2)
            end
          | (act, node') :: edges ->
             (* XXX TODO This isn't robust to the order of the edges *)
            let annot = do_action node act (dfs node' enable_neededness) in
            List.fold_left
            begin fun annot (act, node') ->
            let annot' = do_action node act (dfs node' enable_neededness) in
            (match act with
             (*| Graph.ANotUnique lr ->
                if !lr = LFalse then (Printf.printf "false\n"; ())
                else (Printf.printf "true\n";
                      Potential.constrain annot Eq annot') *)
             | _ -> if possible node' then
                      Potential.constrain annot Eq annot');
            annot
            end annot edges
        in
        begin match annot.(node) with
        | `Doing -> ()
        | `Done a' -> Potential.constrain a Eq a'
        | `Todo -> failwith "unreachable"
        end;
        annot.(node) <- `Done a;
        a)
    in

    let start_annot = dfs body.Graph.g_start enable_neededness in
    begin match !rec_annot with
    | Some annot -> Potential.constrain start_annot Eq annot
    | None -> ()
    end;
    func_annot :=
      Array.mapi (fun i -> function
        | `Done a -> (a, hints.(i))
        | _ -> failwith "impossible dead code"
      ) annot;
    (id, start_annot)

  in (* End of do_fun. *)

  let query = Potential.of_poly query in
  let fstart = List.find (fun f -> f.fun_name = start) fl in
  let wmp =
    match analysis_type with
    | Water_mark -> Some query
    | Size_change -> None
  in
  let (_, start_annot) = do_fun wmp [] start degree query in
  Potential.constrain start_annot Ge pzero;
  match
    let start_node = fstart.fun_body.Graph.g_start in
    let focus = Focus.one :: fstart.fun_focus in
    let focus = find_focus start start_node focus in
    Potential.solve_min (polys_of_focus focus) start_annot
  with
  | None -> None
  | Some sol ->
    let make_poly = Potential.annot_sol sol in

    (* debug: print the annotations *)
    if false then
      List.iter
        begin fun a ->
          Format.eprintf "Dump: %a@."
          Poly.print_ascii (make_poly a)
        end
      !debug_dumps;
    (* end of debug *)

    let annots_final = Hashtbl.create 11 in
    Hashtbl.iter (fun fname (_, func_annot) ->
      let func_annots =
        try Hashtbl.find annots_final fname
        with Not_found -> []
      in
      Hashtbl.replace annots_final fname (
        Array.map (fun (annot, hints) ->
          let hints =
            List.map (fun (fa, f) ->
              (Potential.focus_annot_sol sol fa, f)) hints
          in
          (make_poly annot, hints)
        ) !func_annot :: func_annots
      )
    ) annots;
    Some (annots_final, make_poly start_annot)
