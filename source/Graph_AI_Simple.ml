(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

(* Implementation of a simple abstract domain.
   The advantage is that the outcome is more
   predictable than what we obtain using more
   complex domains.
*)

open Types
open Graph_Types
open Polynom
open CUDA_Types

module L = Presburger.L
module S = Types.IdSet
module M = Map.Make(Id)
(*
module PM = Map.Make(struct type id = cparam option
                            let compare a b =
                              let int_of_dim =
                                function X -> 0
                                       | Y -> 1
                                       | Z -> 2
                              in
                              let int_of_param =
                                function GridDim d -> 1 + int_of_dim d
                                       | BlockIdx d -> 4 + int_of_dim d
                                       | BlockDim d -> 7 + int_of_dim d
                                       | ThreadIdx d -> 10 + int_of_dim d
                                       | WarpSize -> 13
                              in
                              let cvt = function None -> 0
                                               | Some p -> int_of_param p
                              in
                              Int.compare (cvt a) (cvt b))
 *)
module DM = Map.Make (struct type t = dim option
                             let compare a b =
                               let cvt =
                                 function None -> 0
                                        | Some X -> 1
                                        | Some Y -> 2
                                        | Some Z -> 3
                               in
                               (cvt a) - (cvt b)
                      end)

type 'a ob = Bot
           | V of 'a

let args : Types.id list ref = ref []

let glob_allow_extend  = ref false
                
let known_temps : int M.t ref = ref M.empty
(* let waiting_temps : (expr ref list) M.t ref = ref M.empty *)

(* Dependence on thread IDs, possible divergent assignment, uniform *)
type cuda_dom = Poly.t ob DM.t * bool * bool

let monom_var m =
  (* Format.fprintf Format.std_formatter "%a\n"
    (Monom.print ~ascii:false) m; *)
  assert (Monom.degree m = 1);
  match Monom.pick m with
  | (Factor.Var v, _) -> v
  | _ -> assert false

module Translate = struct

  let linear_of_poly p =
    Poly.fold begin fun m k lin ->
      let k = int_of_float k in
      if Monom.is_one m then
        L.addk k lin
      else
        L.set (monom_var m) k lin
    end p (L.const 0)

  let linear_of_expr e =
    let p = Poly.of_expr e in
    if Poly.degree p > 1 then DNF.true_ else
    DNF.lift (linear_of_poly p)

  let dnf_of_logic l =
    let one = mk () (ENum 1) in
    let cmp e1 c e2 =
      match c with
      | Le -> linear_of_expr (mk () (ESub (e1, e2)))
      | Lt -> linear_of_expr (mk () (ESub (mk () (EAdd (one, e1)), e2)))
      | Ge -> linear_of_expr (mk () (ESub (e2, e1)))
      | Gt -> linear_of_expr (mk () (ESub (mk () (EAdd (one, e2)), e1)))
      | Eq ->
        DNF.conjunct
          (linear_of_expr (mk () (ESub (e1, e2))))
          (linear_of_expr (mk () (ESub (e2, e1))))
      | Ne ->
        DNF.disjunct
          (linear_of_expr (mk () (ESub (mk () (EAdd (one, e1)), e2))))
          (linear_of_expr (mk () (ESub (mk () (EAdd (one, e2)), e1))))
    in
    DNF.of_logic cmp l

end

type transfer =
  | TNone
  | TWeaken
  | TGuard of L.sum DNF.t * ulogic
  | TAssign of id * Poly.t option * annot expr ref * bool ref
  | TAddMemReads of id * id * annot * int option ref * int * bool * bool
  | TAddConflicts of id * id * annot * int option ref * int * bool
  | TCall of func * func
  | TReturn of func * func
  | TProb of float
  | TUnique of ulogic ref
  | TNotUnique of ulogic ref
  | TCallUninterp of id * string * id list (* x = f(x1, ..., xn) *)
  | TUnify
  | TSwitch of id list

module HyperGraph = struct

  (* Build the hypergraph required by the fixpoint library. *)

  type info =
    { funch: (id, func) Hashtbl.t
    ; globs: S.t
    }

  type vertex = id * int  (* pair of function name and node id *)
  type hedge = int

  let print_vertex info fmt (vf, vp) =
    let f = Hashtbl.find info.funch vf in
    let pos = f.fun_body.g_position.(vp) in
    Utils.print_position fmt pos

  let from_program (gl, fl) =
    let new_edge =
      let next_edge = ref (-1) in
      fun () -> incr next_edge; !next_edge in

    (* vertex:   type of vertex
       hedge:    type of hyper-edges
       unit:     data associated to the vertex
       transfer: data associated to the hyper-edges
       info:     data associated to the whole graph
    *)
    let funch = Hashtbl.create 51 in
    let globs = List.fold_left (fun s g -> S.add g s) S.empty gl in
    let info = { funch; globs } in
    let g: (vertex, hedge, unit, transfer, info) PSHGraph.t =
      PSHGraph.create PSHGraph.stdcompare 3 info in

    (* Add all the vertexes and fill the info table first. *)
    List.iter begin fun f ->
      Hashtbl.add funch f.fun_name f;
      for node = 0 to Array.length f.fun_body.g_edges - 1 do
        PSHGraph.add_vertex g (f.fun_name, node) ();
      done;
    end fl;

    (* Then add all the edges. *)
    List.iter begin fun f ->
      Array.iteri begin fun src el ->
        List.iter begin fun (act, dst) ->
          let src = f.fun_name, src in
          let dst = f.fun_name, dst in
          match act with
          | ANone ->
            PSHGraph.add_hedge g (new_edge ())
              TNone ~pred:[|src|] ~succ:[|dst|];
          | AUnify ->
            PSHGraph.add_hedge g (new_edge ())
              TUnify ~pred:[|src|] ~succ:[|dst|];
          | ASwitch ids ->
            PSHGraph.add_hedge g (new_edge ())
              (TSwitch ids) ~pred:[|src|] ~succ:[|dst|];
          | AWeaken ->
            PSHGraph.add_hedge g (new_edge ())
              TWeaken ~pred:[|src|] ~succ:[|dst|];
          | AGuard log ->
            let disj = Translate.dnf_of_logic log in
            PSHGraph.add_hedge g (new_edge ())
              (TGuard (disj, log)) ~pred:[|src|] ~succ:[|dst|];
          | AUnique lr ->
            PSHGraph.add_hedge g (new_edge ())
              (TUnique lr) ~pred:[|src|] ~succ:[|dst|];
          | ANotUnique lr ->
            PSHGraph.add_hedge g (new_edge ())
              (TNotUnique lr) ~pred:[|src|] ~succ:[|dst|];
          | AAssign (id, e, br) ->
            let peo =
              match desc !e with
              | ERandom -> None
              | _ -> Some (Poly.of_expr !e)
            in
            PSHGraph.add_hedge g (new_edge ())
              (TAssign (id, peo, e, br)) ~pred:[|src|] ~succ:[|dst|];
          | AAddMemReads (ido, idi, an, ph, size, host, read) ->
             PSHGraph.add_hedge g (new_edge ())
               (TAddMemReads (ido, idi, an, ph, size, host, read))
               ~pred:[|src|] ~succ:[|dst|];
          | AAddConflicts (ido, idi, an, ph, size, read) ->
             PSHGraph.add_hedge g (new_edge ())
               (TAddConflicts (ido, idi, an, ph, size, read))
               ~pred:[|src|] ~succ:[|dst|];
          | AProb p -> 
            PSHGraph.add_hedge g (new_edge ())
              (TProb p) ~pred:[|src|] ~succ:[|dst|];
          | ACall f' ->
            let f' = List.find (fun f -> f.fun_name = f') fl in
            let f'_start = f'.fun_name, f'.fun_body.g_start in
            let f'_end = f'.fun_name, f'.fun_body.g_end in
            PSHGraph.add_hedge g (new_edge ())
              (TCall (f, f')) ~pred:[|src|] ~succ:[|f'_start|];
            PSHGraph.add_hedge g (new_edge ())
              (TReturn (f, f')) ~pred:[|src; f'_end|] ~succ:[|dst|];
          | ACallUninterp (id, f, args) ->
             PSHGraph.add_hedge g (new_edge ())
              (TCallUninterp (id, f, args)) ~pred:[|src|] ~succ:[|dst|];
        end el;
      end f.fun_body.g_edges;
    end fl;
    g

end

let cuda_leq (p1, da1, u1) (p2, da2, u2) =
  ((not da1) || da2) &&
    ((not u2) || u1) &&
  (DM.for_all (fun p v -> try
        match (DM.find p p1, v) with
        | (Bot, _) -> true
        | (_, Bot) -> false
        | (V p1, V p2) -> Poly.compare p1 p2 = 0
      with Not_found -> false)
    p2)

let cuda_unif = (DM.add (Some X) (V (Poly.zero ()))
                   (DM.add (Some Y) (V (Poly.zero ()))
                      (DM.add (Some Z) (V (Poly.zero ())) DM.empty)),
                 false,
                 true)

let cuda_unif_of u p =
  (DM.add (Some X) (V (Poly.zero ()))
     (DM.add (Some Y) (V (Poly.zero ()))
        (DM.add (Some Z) (V (Poly.zero ()))
           (DM.add None (V p) DM.empty))),
   false,
   u)

let cuda_is_unif (m, _, _) =
  List.for_all
    (fun p -> try
        match DM.find p m with
        | Bot -> false
        | V p -> Poly.compare p (Poly.zero ()) = 0
      with Not_found -> false)
    [Some X; Some Y; Some Z]

let cuda_div = (DM.empty, false, false)

let cuda_join id (p1, da1, u1) (p2, da2, u2) =
  (*if da1 || da2 then cuda_div (* Maybe too conservative? *)
  else *)
  if cuda_is_unif (p1, da1, u1) && cuda_is_unif (p2, da2, u2) then
    match (DM.find_opt None p1, DM.find_opt None p2) with
    | (Some (V p1), Some (V p2)) ->
       if Poly.compare p1 p2 = 0 then cuda_unif_of (u1 && u2) p1
       else cuda_unif_of (u1 && u2) (Poly.of_expr (mk () (EVar id)))
    | _ -> cuda_unif_of (u1 && u2) (Poly.of_expr (mk () (EVar id)))
  else
  (DM.merge (fun p a b ->
       match (a, b) with
       | (None, _) -> None
       | (_, None) -> None
       | (Some Bot, b) -> b
       | (_, Some Bot) -> a
       | (Some (V av), Some (V bv)) ->
          if Poly.compare av bv = 0 then Some (V av) else None)
     p1 p2,
   da1 || da2,
   u1 && u2)


let cuda_bot = (DM.add (Some X) Bot
                  (DM.add (Some Y) Bot
                     (DM.add (Some Z) Bot
                        (DM.add None Bot DM.empty))), false, true)

let get_cd m id =
  try M.find id m
  with Not_found -> cuda_unif_of true (Poly.of_expr (mk () (EVar id)))

let print_cuda fmt (m, _, _) =
  let print_dim first fmt d =
    (if not first then Format.fprintf fmt  " + ");
    (match DM.find_opt d m with
     | Some (V p) -> Poly.print fmt p
     | Some Bot -> Format.fprintf fmt "!"
     | None -> Format.fprintf fmt "?");
    Format.fprintf fmt "%s"
      (match d with
       | None -> "" | Some X -> "tx" | Some Y -> "ty" | Some Z -> "tz")
  in
  Format.fprintf fmt "%a%a%a%a"
    (print_dim true) (Some X)
    (print_dim false) (Some Y)
    (print_dim false) (Some Z)
    (print_dim false) None

let cfst (a, _, _) = a
let csnd (_, a, _) = a
let ctd (_, _, a) = a

let cuda_of_var d = ((DM.add (Some d) (V (Poly.const 1.0))
                        (DM.add None (V (Poly.const 0.0)) (cfst cuda_unif))),
                     false,
                     false)

let cuda_init = List.fold_left (fun m (id, v) -> M.add id v m) M.empty
                   [(CUDA_Config.tidx_var, cuda_of_var X);
                    (CUDA_Config.tidy_var, cuda_of_var Y);
                    (CUDA_Config.tidz_var, cuda_of_var Z)]

(* Presburger constraints, Extended Presburger constraints,
CUDA domain, possibly divergent *)
type absval = (Presburger.L.sum list) * (Presburger.L.sum list) *
                (cuda_dom M.t) * bool
let bottom = (Presburger.bottom, Presburger.bottom, cuda_init, false)
let is_bottom (_, p, _, _) =
  (not (Presburger.sat p))(*  && (s = None) && (m = None)*)
let is_leq (p1, p1e, m1, d1) (p2, p2e, m2, d2) =
  (List.for_all (Presburger.implies p1) p2) &&
    (List.for_all (Presburger.implies p1e) p2e) &&
      (M.for_all (fun id cd -> cuda_leq cd (get_cd m2 id)) m1) &&
        ((not d1) || d2)

let join (p1, p1e, m1, dv1) (p2, p2e, m2, dv2) =
  (Presburger.join p1 p2,
   Presburger.join p1e p2e,
   M.merge (fun id d1 d2 ->
       let d1 = match d1 with Some x -> x | None -> cuda_bot in
       let d2 = match d2 with Some x -> x | None -> cuda_bot in
       Some (cuda_join id d1 d2))
     m1 m2,
  dv1 || dv2)

let join_list = List.fold_left join bottom
let top = ([], [], M.empty, true)
let print fmt (_, p, m, _) =
  let open Format in
  (fprintf fmt "%a" Presburger.print p;
   (Print.list ~sep:" &&@ "
     (fun fmt (id, cd) ->
       (fprintf fmt "%s = %a" id print_cuda cd))
     fmt
     (M.bindings m))
  )

let bd_to_CUDA neg (k, l) =
  let open CUDA_Types in
  let k = if neg then ~-k else k in
  if k = 1 then
    Presburger.L.toCUDA l
  else if k = ~-1 then
    emk () (CMul (Presburger.L.toCUDA l, emk () (CConst (CInt ~-1))))
  else
    emk () (CDiv (Presburger.L.toCUDA l, emk () (CConst (CInt k))))

let bd_to_poly neg (k, l) =
  let module P = Polynom.Poly in
  let k = if neg then ~-k else k in
  P.scale (1. /. (float_of_int k)) (Presburger.L.toPoly l)
  

let is_nonneg (abs, _, _, _) pol =
  if Poly.degree pol > 1 then false else
  let l = L.mult (-1) (Translate.linear_of_poly pol) in
  List.exists (L.eq l) abs || Presburger.implies abs l

  
module Solver = struct

  (* Here we use the fixpoint library to find an
     abstract state for each vertex of the hypergraph.
   *)

  let is_param id =
    let is_reg n =
      match desc (CUDA.lookup_var ("_reg_" ^ (string_of_int n))) with
      | EVar x -> x = id
      | _ -> false
    in
    let b = List.mem id CUDA_Config.cuda_vars ||
              (String.length id > 7 && String.sub id 0 7 = "__param") ||
                (String.length id > 5 && String.sub id 0 5 = "_reg_") ||
                  List.exists is_reg [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
    in
    b
    

  let is_nonneg_lb (k, l) =
    k > 0 && (L.get_const l >= 0)
    && L.fold_vars (fun v c p -> p && is_param v && c >= 0) true l

  let is_nonneg_lbs lbs =
    List.exists is_nonneg_lb lbs
(* Old
let bounds abs pe =
    Poly.fold begin fun m k (lbs, ubs) ->
      let k = int_of_float k in
      let addk k (k', l) = (k', L.addk (k*k') l) in
      if Monom.is_one m then
        (List.map (addk (+k)) lbs, List.map (addk (-k)) ubs)
      else
        let v = monom_var m in
        let c_comp f l = f (L.coeff v l) 0 in
        let ubs' = List.filter (c_comp (>)) abs in
        let lbs' = List.filter (c_comp (<)) abs in
        let ubs', lbs', k =
          if k < 0 then (lbs', ubs', -k) else (ubs', lbs', +k)
        in
        let addscale l (k', l') =
          let vk = Pervasives.abs (L.coeff v l) in
          let l = L.mult k l in
          let lcm = Presburger.lcm vk k' in
          let l = L.plus (lcm/vk) l (L.mult (lcm/k') l') in
          (lcm, L.set v 0 l)
        in
        let merge a a' =
          List.concat
            (List.map (fun l -> List.map (addscale l) a) a')
        in
        (merge lbs lbs', merge ubs ubs')
      end pe ([(1, L.const 0)], [(1, L.const 0)])
 *)
  let bounds ?(allow_params = false) abs pe =
    Poly.fold begin fun m k (lbs, ubs) ->
      let k = int_of_float k in
      let addk k (k', l) = (k', L.addk (k*k') l) in
      let addv id n (k, l) = (k, L.addl id n l) in
      if Monom.is_one m then
        (List.map (addk (+k)) lbs, List.map (addk (-k)) ubs)
      else
        (* if Monom.degree m > 1 then ([], [])
        else *)
        let v = monom_var m in
        if is_param v && allow_params && !glob_allow_extend then
           (List.map (addv v 1) lbs, List.map (addv v (-1)) ubs)
        else
            let c_comp f l = f (L.coeff v l) 0 in
            let ubs' = List.filter (c_comp (>)) abs in
            let lbs' = List.filter (c_comp (<)) abs in
            let ubs', lbs', k =
              if k < 0 then (lbs', ubs', -k) else (ubs', lbs', +k)
            in
            let addscale l (k', l') =
              let vk = Pervasives.abs (L.coeff v l) in
              let l = L.mult k l in
              let lcm = Presburger.lcm vk k' in
              let l = L.plus (lcm/vk) l (L.mult (lcm/k') l') in
              (lcm, L.set v 0 l)
            in
            let merge a a' =
              List.concat
                (List.map (fun l -> List.map (addscale l) a) a')
            in
            (merge lbs lbs', merge ubs ubs')
      end pe ([(1, L.const 0)], [(1, L.const 0)])

  let factor_var f =
    match f with
    | Factor.Var v -> v
    | _ -> failwith "factor not a var"
    
  let bounds_gen (abs, cu) pe =
    (*
    let _ =
      let open Format in
      ((Print.list ~sep:" &&@ "
          (fun fmt (id, cd) ->
            (fprintf fmt "%s = %a" id print_cuda cd))
          std_formatter
          (M.bindings cu))
      )
    in
     *)
    let rec bounds_gen_poly depth pe =
      if depth <= 0 then None
      else
    let open CUDA_Types in
    let filter_bds f (_, b) =
      let vars = L.vars Presburger.S.empty b in
      Presburger.S.for_all
        (fun v -> is_param v
                  || Factor.var_exists (fun v' -> v' = v) f) vars
    in
    let cadd (e1, e2) = emk () (CAdd (e1, e2)) in
    let cmul (e1, e2) = emk () (CAdd (e1, e2)) in
    let cconst e = emk () (CConst e) in
    (*
    let _ = Format.fprintf Format.std_formatter "bounds_gen %a\n"
              Poly.print pe
    in
     *)
    let add_opt b1 b2 =
      match (b1, b2) with
      | (Some b1, Some b2) -> Some (Poly.add b1 b2)
      | _ -> None
    in
    let add2_opt (lb1, ub1) (lb2, ub2) =
      (add_opt lb1 lb2, add_opt ub1 ub2)
    in
    let mul_opt b1 b2 =
      match (b1, b2) with
      | (Some b1, Some b2) -> Some (Poly.mul b1 b2)
      | _ -> None
    in
    let sc_r_f_opt f fb1 arg b2 =
      match b2 with
        None -> None
      | Some (lb2, ub2) ->
         (match fb1 arg with
          | None -> None
          | Some (lb1, ub1) -> Some (f lb1 lb2, f ub1 ub2)
         )
    in
    let sc_r_add_opt = sc_r_f_opt Poly.add in
    let sc_r_mul_opt = sc_r_f_opt Poly.mul in
    let bounds_from_cuda id =
      (* Try using the CUDA abstraction domain to get bounds, e.g.
       * if x = 2tidx + k, then lb(x) = ub(x) = k *)
      let get_dim_bds (dim, var) =
        try
          let (cm, _, _) = M.find id cu in
          match DM.find dim cm with
          | Bot -> None
          | V p ->
             (match bounds_gen_poly (depth - 1) p with
              | None -> None
              | Some (lb, ub) ->
                 let poly_var = match var with
                   | Some var -> Poly.of_monom (Monom.of_var var) 1.0
                   | None -> Poly.const 1.0
                 in
                 Some (Poly.mul lb poly_var, Poly.mul ub poly_var)
             )
        with Not_found -> None
      in
      let open CUDA_Config in
      sc_r_add_opt get_dim_bds (Some X, Some tidx_var)
        (sc_r_add_opt get_dim_bds (Some Y, Some tidy_var)
           (sc_r_add_opt get_dim_bds (Some Z, Some tidz_var)
              (get_dim_bds (None, None))))
    in
    let bounds_from_presb f e =
      (* Bring out the big hammer and try to get bounds using the
       * Presburger constraints
       * (actually, this hammer is of incomparable size with the CUDA one,
       * since each will find some bounds the other won't)
       *)
      let (lbs, ubs) =
        bounds abs
          (Poly.of_monom (Monom.of_factor f e) 1.0)
      in
      (*
      Format.fprintf Format.std_formatter "%d lbs, %d ubs\n"
        (List.length lbs)
        (List.length ubs);
       *)
      match (List.filter (filter_bds f) lbs,
             List.filter (filter_bds f) ubs)
      with
      | (lb'::olbs, ub'::_) ->
         if is_nonneg_lbs (lb'::olbs)
            || e mod 2 = 0 then
           let (plb, pub) = (bd_to_poly false lb',
                             bd_to_poly true ub')
           in
           (*
           let _ = Format.fprintf Format.std_formatter
                     "lb: %a\nub: %a\n"
                     Poly.print plb
                     Poly.print pub
           in
            *)
           Some (plb, pub)
         else
           ((*Format.fprintf Format.std_formatter "Negative lb\n";*)
            None)
      | _ -> None
    in
    Poly.fold (fun m k ->
        sc_r_f_opt (fun a b -> Poly.add b (Poly.scale k a))
          (fun _ ->
            Monom.fold (fun f e ->
                sc_r_mul_opt
                  (fun _ ->
                    if Factor.degree f = 1 && e = 1
                    then
                      let v = factor_var f in
                      if is_param (factor_var f) then
                        (* This is a CUDA builtin or function parameter, we
                         * can use it as its own bound *)
                        let v = factor_var f in
                        Some (Poly.of_monom (Monom.of_var v) 1.0,
                              Poly.of_monom (Monom.of_var v) 1.0)
                      else
                        (* First try using the CUDA abstraction domain *)
                        match bounds_from_cuda v with
                        | Some bds -> Some bds
                        | None -> bounds_from_presb f e
                    else
                      (* If that fails or isn't applicable, use Presburger *)
                      bounds_from_presb f e
                  )
                  ()
              )
              m
              (Some (Poly.of_monom Monom.one 1.0, Poly.of_monom Monom.one 1.0))
          )
          ()
      )
      pe
      (Some (Poly.zero (), Poly.zero ()))
    in
    match bounds_gen_poly 2 pe with
    | Some (plb, pub) ->
       (match (Poly.toCUDA plb, Poly.toCUDA pub)
        with
        | (Some clb, Some cub) ->
           (
             (* Format.fprintf Format.std_formatter "Bounds for %a:\nlb: %a\nub: %a\n"
              Poly.print pe
              CUDA.print_cexpr clb
              CUDA.print_cexpr cub; *)
            Some (clb, cub))
        | _ -> (*Format.fprintf Format.std_formatter "None\n";*) None)
    | None -> ((*Format.fprintf Format.std_formatter "None\n";*) None)

  let make_fpmanager graph widening apply =
    let info = PSHGraph.info graph in
    let dont_print _ _ = () in
    { Fixpoint.bottom = (fun _ -> bottom)
    ; canonical = (fun _ _ -> ())
    ; is_bottom = (fun _ -> is_bottom)
    ; is_leq = (fun _ -> is_leq)
    ; join = (fun _ -> join)
    ; join_list = (fun _ -> join_list)
    ; odiff = None
    ; widening = (fun _ -> widening)
    ; abstract_init =
        (fun _ -> ([], [], cuda_init, false))
    ; arc_init = (fun _ -> ())
    ; apply = apply graph
    ; print_vertex = HyperGraph.print_vertex info
    ; print_hedge = Format.pp_print_int
    ; print_abstract = print
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

  (* Bounds was here *)

  let var_is_unique ((_, abs, m, _) as ab) id =
    let (lbs, ubs) = bounds abs (Poly.of_monom (Monom.of_var id) 1.0) in
    let boundplusone (k, l) =
      L.plus (-1) (L.set id k (L.const 0)) (L.plus 1 (L.const 1) l)
    in
    let pluslbs = List.map boundplusone lbs in
    List.exists (fun lb -> not (Presburger.sat (lb::abs))) pluslbs

  let rec e_is_unique ((_, abs, m, _) as ab) expr =
    (* let _ = Printf.printf "e_is_unique\n%!" in *)
    match desc expr with
    | EVar id ->
       let un = (cuda_is_unif (get_cd m id)) (* || (var_is_unique id) *)
       in
       (*let _ = Printf.printf "%s is %sunique\n%!" id
                 (if un then "" else "not ")
       in *)
       un
    | ENum _ -> ((*Printf.printf "num\n%!"; *)true)
    | EAdd (e1, e2)
      | ESub (e1, e2)
      | EMul (e1, e2) -> (e_is_unique ab e1) && (e_is_unique ab e2)
    | _ -> ((*Printf.printf "not sure\n%!"; *) false)

  let rec e_is_pot ((_, abs, m, _) as ab) expr =
    match desc expr with
    | EVar id -> ctd (get_cd m id)
    | EAdd (e1, e2)
      | ESub (e1, e2)
      | EMul (e1, e2) -> (e_is_pot ab e1) && (e_is_pot ab e2)
    | _ -> true

  let rec is_unique ((_, abs, m, _) as ab) log =
    match log with
    | LTrue | LFalse -> true
    | LRandom -> false
    | LCmp (e1, _, e2) -> (e_is_unique ab e1) && (e_is_unique ab e2)
    | LAnd (l1, l2) | LOr (l1, l2) -> (is_unique ab l1) && (is_unique ab l2)
    | LNot l -> is_unique ab l

  let apply_TGuard ((abs, abse, m, d) as ab) (disj, l) =
    let abs_and_disj = List.map (Presburger.meet abs) disj in
    let abse_and_disj = List.map (Presburger.meet abse) disj in
    let d' = d || not (is_unique ab l) in
    let abs' =
      match abs_and_disj with
      | [] -> Presburger.bottom
      | [x] -> x
      | x :: disj -> List.fold_left Presburger.join x disj
    in
    let abse' =
      match abse_and_disj with
      | [] -> Presburger.bottom
      | [x] -> x
      | x :: disj -> List.fold_left Presburger.join x disj
    in
    (abs', abse', m, d')

  let apply_TNotUnique abs lr =
    (* let _ = Printf.printf "apply_TNotUnique\n%!" in *)
    if is_unique abs (!lr) then
      ((* lr := LFalse; *)
       (* Printf.printf "false\n%!"; *)
       bottom)
    else
      ((* Printf.printf "true\n"; *)
       (* lr := LTrue; *)
       abs)

  let find_poly dim dom =
    match DM.find dim dom with
    | Bot -> raise Not_found
    | V p -> p

  let dom_is_const (d, _, _) =
    try
      if (Poly.compare (find_poly (Some X) d) (Poly.zero ()) = 0) &&
           (Poly.compare (find_poly (Some Y) d) (Poly.zero ()) = 0) &&
             (Poly.compare (find_poly (Some Z) d) (Poly.zero ()) = 0)
      then
        match DM.find_opt None d with
        | Some (V p) -> Some (Poly.is_const p)
        | Some Bot -> Some None
        | None -> Some None
      else None
    with Not_found -> None

  let vars_of_poly p =
    Poly.fold (fun m c l -> (monom_var m, c)::l) p []

  let dom_is_param (d, _, _) =
    try
      let (px, py, pz, pc) =
        (find_poly (Some X) d,
         find_poly (Some Y) d,
         find_poly (Some Z) d,
         find_poly None d)
      in
      match
        (Poly.compare px (Poly.zero ()) <> 0,
         Poly.compare py (Poly.zero ()) <> 0,
         Poly.compare pz (Poly.zero ()) <> 0,
         Poly.compare pc (Poly.zero ()) <> 0)
      with
      | (false, false, false, true) -> (* is constant *)
         (match vars_of_poly pc with
          | [s, c] -> if String.compare s CUDA_Config.warpsize_var = 0 then
                        Some (CUDA_Types.WarpSize, c)
                      else None
          | _ -> None)
      | (true, false, false, false) ->
         (match Poly.is_const px with
          | Some c -> Some (CUDA_Types.ThreadIdx X, c)
          | None -> None)
      | (false, true, false, false) ->
         (match Poly.is_const py with
          | Some c -> Some (CUDA_Types.ThreadIdx Y, c)
          | None -> None)
      | (false, false, true, false) ->
         (match Poly.is_const pz with
          | Some c -> Some (CUDA_Types.ThreadIdx Z, c)
          | None -> None)
      | _ -> None
    with Not_found -> None

  let rec dom_of_expr m e =
    let combine_doms c_poly (d1, da1, u1) (d2, da2, u2) =
      (DM.merge (fun _ p1 p2 ->
          match (p1, p2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some Bot, _) -> p2
          | (_, Some Bot) -> p1
          | (Some (V p1), Some (V p2)) -> Some (V (c_poly p1 p2))
          | _ -> None)
         d1 d2,
       da1 || da2,
       u1 && u2)
    in
    let map f (m, da, u) =
      (DM.map (fun x -> match x with
                          Bot -> Bot
                        | V p -> V (f p))
         m,
       da,
       u)
    in
    match desc e with
    | EVar id -> get_cd m id
    | EAdd (e1, e2) ->
       combine_doms Poly.add
         (dom_of_expr m e1) (dom_of_expr m e2)
    | ESub (e1, e2) ->
       combine_doms Poly.sub
         (dom_of_expr m e1) (dom_of_expr m e2)
    | EMul (e1, e2) ->
       let de1 = dom_of_expr m e1 in
       let de2 = dom_of_expr m e2 in
       if cuda_is_unif de1 && cuda_is_unif de2 then
         ((* Printf.printf "unif, unif\n"; *)
         match (DM.find_opt None (cfst de1), DM.find_opt None (cfst de2)) with
         | (Some (V p1), Some (V p2)) ->
            ((* Printf.printf "both\n"; *)
             (cfst (cuda_unif_of (ctd de1 && ctd de2) (Poly.mul p1 p2)),
              (csnd de1 || csnd de2),
              (ctd de1 && ctd de2)))
         | _ -> (cfst cuda_unif,
                 (csnd de1 || csnd de2),
                 (ctd de1 && ctd de2)))
       else
       (match (dom_is_const de1, dom_is_const de2) with
        | (Some (Some f), _) -> map (Poly.mul (Poly.const f)) de2
        | (Some None, _) -> (DM.filter (fun _ v ->
                                 match v with
                                 | V p -> Poly.compare p (Poly.zero ()) = 0
                                 | _ -> false)
                               (cfst de2),
                             csnd de2,
                             false)
        | (_, Some (Some f)) -> map (Poly.mul (Poly.const f)) de1
        | (_, Some None) -> ((* Printf.printf "mul\n"; *)
                             (DM.filter (fun _ v ->
                                  match v with
                                  | V p -> Poly.compare p (Poly.zero ()) = 0
                                  | _ -> false)
                                (cfst de1)),
                             csnd de2,
                             false)
        | (None, None) -> cuda_div (*
           if cuda_is_unif de1 && cuda_is_unif de2 then
             cuda_unif
           else
             cuda_div *)
       )
    | ENum n -> (DM.add None (V (Poly.const (float_of_int n))) (cfst cuda_unif),
                 false,
                 true)
    | _ -> cuda_unif

  let apply_TAssign (f: ?e:annot Types.expr ref -> Types.id * int -> absval -> unit)
        params (abs, abse, m, d) id (peo, (e : annot Types.expr ref), is_pot) =
    (* let _ = f ~e:e ("", 0) (abs, abse, m, d) in *)
    let an = ann !e in
    let _ = match (params, desc !e) with
      | (Some (_, metric), EAdd ((_, EVar ov), (_, EVar v))) ->
         if String.compare v CUDA_Config.div_cost = 0
         then
           e := mk (an) (EAdd (mk (an) (EVar ov),
                             mk (an) (ENum (metric CUDA_Cost.KDivWarp))))
         else
           (match M.find_opt v !known_temps with
            | Some n -> ((* Printf.printf "found\n"; *)
                         e := mk (an) (EAdd (mk (an) (EVar ov), mk (an) (ENum n))))
            | None -> () (*
                      let l = match M.find_opt v !waiting_temps with
                        | Some l -> l
                        | None -> []
                      in
                      (* Printf.printf "adding me for %s\n" v; *)
                      waiting_temps := M.add v (e::l) !waiting_temps *))
      | _ -> ()
    in
    let _ =
      is_pot := !is_pot ||
                  ((ctd (get_cd m id))
                   && (e_is_unique (abs, abse, m, d) (!e))
                   && (e_is_pot (abs, abse, m, d) (!e)))
    in
    let forget_id abs =
      List.filter (fun l -> L.coeff id l = 0) abs in
    let update_abs ext abs =
      if peo = None then forget_id abs else
    let pe = match peo with Some x -> x | _ -> assert false in

    (* Linear assignment, we consider two cases:
       1. If the assignment is an increment "x = x+i"
          we can substitute the assigned variable with
          "x-i".
       2. Otherwise, "x = e", we find upper and lower
          bounds on e and add these to x.
    *)
    if Poly.degree pe <= 1 then
      let sign x = if x < 0 then (-x, -1) else (+x, +1) in
      let k = Poly.get_coeff (Monom.of_var id) pe in
      let k, ks = sign (int_of_float k) in

      if k <> 0 then
        (* Case 1. *)
        let abs', abs =
          List.partition (fun i -> L.coeff id i <> 0) abs
        in
        let abs' =
          let i' = Translate.linear_of_poly pe in
          List.map begin fun i ->
            let idk, idks = sign (L.coeff id i) in
            let l = Presburger.lcm idk k in
            let i = L.mult (l/idk) i in
            if ks = idks then
              let i = L.plus (-l/k) i' i in
              assert (L.coeff id i = 0);
              L.set id (l/k) i
            else
              let i = L.plus (l/k) i' i in
              assert (L.coeff id i = 0);
              L.set id (-l/k) i
          end abs'
        in
        List.rev_append abs' abs

      else
        (* Case 2. *)
        let abs = forget_id abs in
        let lbs, ubs = bounds ~allow_params:ext abs pe
        in
        (*
        let _ = Format.fprintf Format.std_formatter "pe: %a (%d, %d)\n"
                  Poly.print pe
                  (List.length lbs)
                  (List.length ubs)
        in
         *)
        (*
        let _ = (Format.fprintf Format.std_formatter "lbs: ";
                 (match bound_to_CUDA false lbs with
                  | Some e -> CUDA.print_cexpr Format.std_formatter e
                  | None -> ())
                 ;
                   Format.print_newline ())
        in
        let _ = (Format.fprintf Format.std_formatter "ubs: ";
                 (match bound_to_CUDA true ubs with
                  | Some e -> CUDA.print_cexpr Format.std_formatter e
                  | None -> ());
                 Format.print_newline ())
        in
         *)
        let bound low (k, l) =
          let coeff = if low then (-1) else (+1) in
          L.plus coeff (L.set id k (L.const 0)) l
        in
        let lbs = List.map (bound true) lbs in
        let ubs = List.map (bound false) ubs in
        (*(Translate.linear_of_poly
           (Poly.sub (Poly.of_monom (Monom.of_var id) 1.0) pe))::
          (Translate.linear_of_poly
             (Poly.sub pe (Poly.of_monom (Monom.of_var id) 1.0)))::*)
            (List.rev_append lbs (List.rev_append ubs abs))

    (* General polynomial assignment, the best we
       we can do is bound independently each monom
       by constants and aggregate the bounds.
    *)
    else
      forget_id abs
    in
    (update_abs false abs,
     update_abs true abse,
     (let (dm, da, u) = dom_of_expr m (!e) in
       (* let (_, _, u) = get_cd m id in *)
       M.add id (dm, da || d, u) m),
    d)

  (*
  let apply_TAssign abs id peo =
    let res = apply_TAssign abs id peo in
    Format.eprintf "apply_TAssign(%a) = %a@."
      Presburger.print abs Presburger.print res;
    res
  *)

  let apply_TCall gs (abs, abse, _, d) =
    (List.filter (fun s -> S.subset (L.vars S.empty s) gs) abs,
     List.filter (fun s -> S.subset (L.vars S.empty s) gs) abse,
     cuda_init, d)

  let apply_TReturn gs (abs_caller, abse_caller, _, d1)
        (abs_callee, abse_callee, _, d2) caller =
    let ls = List.fold_left (fun ls v -> S.add v ls) S.empty caller.fun_vars in
    (List.filter (fun s -> S.subset (L.vars S.empty s) gs) abs_callee @
       List.filter (fun s -> S.subset (L.vars S.empty s) ls) abs_caller
     ,
       List.filter (fun s -> S.subset (L.vars S.empty s) gs) abse_callee @
         List.filter (fun s -> S.subset (L.vars S.empty s) ls) abse_caller
     , cuda_init
     , d1 || d2
   )

  let fixParams _ =
    let fixedVal id =
      if String.compare id CUDA_Config.warpsize_var = 0 then 32 else 5
    in
    List.fold_left
      (fun l id -> (L.set id 1 (L.const (fixedVal id)))::
                     (L.set id (-1) (L.const (fixedVal id)))::l)
      []
      ["wA"] (*
      CUDA_Config.[warpsize_var;
                   bidx_var;
                   bidy_var;
                   bidz_var] *)
  (* (S.elements s) *)

  let (/^) a b = a / b + (if a mod b = 0 then 0 else 1)

  let const_bounds p v = (None, None) (*
    (* Presburger.range p v *)
    let (lbs, ubs) = bounds p (Poly.of_monom (Monom.of_var v) 1.0) in
    let _ = Printf.printf "%d lbs, %d ubs\n" (List.length lbs) (List.length ubs)
    in
    let extract_const low (k, l) =
      if IdSet.is_empty (L.vars IdSet.empty l)
      then
        let k' = float_of_int l.k in
        let k = float_of_int k in
        let _ = Printf.printf "%f / %f\n" k' k in
        if low then Some (int_of_float (k' /. k))
        else Some (int_of_float (0. -. k' /. k))
      else
        let _ = Printf.printf "%d vars\n" (IdSet.cardinal (L.vars IdSet.empty l))
        in
        None
    in
    (List.fold_left (fun m lb ->
         let c = extract_const true lb
         in
         match (m, c) with
         | (None, _) -> c
         | (_, None) -> m
         | (Some c1, Some c2) -> Some (max c1 c2))
       None
       lbs,
     List.fold_left (fun m lb ->
         let c = extract_const false lb
         in
         match (m, c) with
         | (None, _) -> c
         | (_, None) -> m
         | (Some c1, Some c2) -> Some (min c1 c2))
       None
       ubs) *)

  let values_of_x p params =
    let open CUDA_Cost in
    let (l, u) = const_bounds p CUDA_Config.tidx_var in
    ((match l with
      | None -> 0
      | Some l -> max 0 l),
     (match u with
      | None -> min params.warp_size params.blockdim.x
      | Some u -> min (min params.warp_size params.blockdim.x)
                    u))

  let values_of_y p params =
    let open CUDA_Cost in
    let vy = min (params.warp_size /^ params.blockdim.x)
               params.blockdim.y
    in
    let (l, u) = const_bounds p CUDA_Config.tidy_var in
    ((match l with
      | None -> 0
      | Some l -> max 0 l),
     (match u with
      | None -> vy
      | Some u -> min vy u))

  let values_of_z p params =
    let open CUDA_Cost in
    let vz = min (params.warp_size /^
                    (params.blockdim.x * params.blockdim.y))
               params.blockdim.z
    in
    let (l, u) = const_bounds p CUDA_Config.tidz_var in
    ((match l with
      | None -> 0
      | Some l -> max 0 l),
     (match u with
      | None -> vz
      | Some u -> min vz u))

  let apply_MemReads params (p, pext, m, dv) ido idi ph type_size host read =
    let (params, metric) = match params with
      | Some x -> x
      | None -> failwith "Can't do CUDA analysis without parameters"
    in
    let open CUDA_Cost in
    let nonzero_tid = ref false in
    let values_of_x =
      let (l, u) = values_of_x p params in
      ((if l > 0 then nonzero_tid := true);
       u - l)
    in
    let values_of_y =
      let (l, u) = values_of_y p params in
      (if l > 0 then nonzero_tid := true);
      u - l
    in
    let values_of_z =
      let (l, u) = values_of_z p params in
      (if l > 0 then nonzero_tid := true);
      u - l
    in
    let (lbs, ubs) = bounds p (Poly.of_monom (Monom.of_var idi) 1.0) in
    let bound low (k, l) =
      let coeff = if low then (-1) else (+1) in
      let id = if low then "__lower" else "__upper" in
      L.plus coeff (L.set id k (L.const 0)) l
    in
    let lbs = List.map (bound true) lbs in
    let ubs = List.map (bound false) ubs in
    let p' = List.rev_append lbs (List.rev_append ubs p) in
    let boundedby bound =
      Presburger.implies p'
        (L.set "__upper" 1 (L.set "__lower" (-1) (L.const (-bound))))
    in
    let rec bound b =
      if b >= 32 then 32 else
      if boundedby (b * 4) then b + 1
      else bound (b + 1)
    in
    let bp = bound 1 in
    let bd =
      try
        let p = get_cd m idi in
        (* let _ = Printf.printf "bd\n" in *)
        let cx = find_poly (Some X) (cfst p) in
        let cy = if params.warp_size > params.blockdim.x then
                   try
                     find_poly (Some Y) (cfst p)
                   with _ -> Poly.const 1000.
                 else Poly.const 0.0
        in
        let cz = if params.warp_size > params.blockdim.x * params.blockdim.y
                 then
                   try
                     find_poly (Some Z) (cfst p)
                   with _ -> Poly.const 1000.
                 else Poly.const 0.0
        in
        let c = try find_poly None (cfst p)
                with Not_found -> Poly.const 3.14159 in
        (* XXX TODO type size *)
        let vcx = (* Variation coming from the coefficient of threadIdx.x *)
          match Poly.is_const cx with
          | Some cx ->
             cx *. (float_of_int values_of_x)
          (*  (float_of_int (min params.warp_size params.blockdim.x)) *)
          | None -> 1000.
        in
        let vcy =
          match Poly.is_const cy with
          | Some cy ->
             cy *. (float_of_int values_of_y) (*
               (float_of_int (min ((params.warp_size / params.blockdim.x) - 1)
                                params.blockdim.y)) *)
          | None -> 1000.
        in
        let vcz =
          match Poly.is_const cz with
          | Some cz ->
             cz *. (float_of_int values_of_z) (*
               (float_of_int
                  (min ((params.warp_size /
                           (params.blockdim.x * params.blockdim.y)) - 1)
                     params.blockdim.z)) *)
          | None -> 1000.
        in
        let (vcx, vcy, vcz) = (ceil vcx, ceil vcy, ceil vcz) in
        let (vcx, vcy, vcz) =
          (int_of_float vcx, int_of_float vcy, int_of_float vcz)
        in
        let read_size = if host
                        then params.host_read_size
                        else params.global_read_size
        in
        let may_be_misaligned =
          if
            Poly.fold (fun _ c b ->
                b || ((int_of_float c) * type_size mod read_size <> 0))
              c
              false
          then 1
          else 0
        in
        (* let _ = Printf.printf "vcx = %d, vcy = %d, vcz = %d\n" vcx vcy vcz in *)
        let v1 = let var = (max 1 (vcx + vcy + vcz)) * type_size
                 in var /^ read_size
                    + may_be_misaligned
        in
        let v2 = (let var = (max 1 vcx) * type_size
                  in var /^ read_size
                     + may_be_misaligned) *
                   (max 1 (params.warp_size / params.blockdim.x))
        in
        let v3 = (let var = (max 1 (vcx + vcy)) * type_size
                  in var /^ read_size
                     + may_be_misaligned) *
                   (max 1
                      (params.warp_size / (params.blockdim.x * params.blockdim.y)))
        in
        (* let _ = Printf.printf "v1: %d, v2: %d, v3: %d\n" v1 v2 v3 in *)
        min (min (min v1 v2) v3) 32
      with Not_found -> 32
    in
    (* let _ = Printf.printf "bound: %d\n" bd in *)
    let mem = if host then Host else Global in
    let b = metric (if read then CUDA_Cost.KArrRead (mem, min bd bp)
                    else CUDA_Cost.KArrAssign (mem, min bd bp))
    in
    (* let _ = known_temps := M.add ido b !known_temps in *)
    (* let _ = match M.find_opt ido !waiting_temps with
      | Some refs -> ((* Printf.printf "found waiting\n"; *)
                      List.iter
                        (fun e ->
                          match !e with
                          | EAdd (EVar ov, EVar _) ->
                             (* Printf.printf "setting\n"; *)
                             e := EAdd (EVar ov, ENum b))
                       refs)
      | None -> ()
    in*)
    (* let _ = Printf.printf "filling in %s\n" idi in *)
    let _ = ph := Some b in
    (p, pext, m, dv)
  (* ((L.set ido (1) (L.const (-b)))::p, m) *)

  let mult_of_tid params p =
    try
      (* XXX TODO *)
      let open CUDA_Cost in
      let cx = match Poly.is_const (find_poly (Some X) p) with
        | Some cx -> if cx < 0.99 then raise Not_found
                     else cx
        | None -> raise Not_found
      in
      let _ = if params.warp_size > params.blockdim.x then
                match Poly.is_const (find_poly (Some Y) p) with
                | Some cy ->
                   let cy = cy /. (float_of_int params.blockdim.x) in
                   if abs_float (cx -. cy) < 0.01 then ()
                   else raise Not_found
                | None -> raise Not_found
              else ()
      in
      let _ = if params.warp_size > params.blockdim.x * params.blockdim.y
              then
                match Poly.is_const (find_poly (Some Z) p) with
                | Some cz ->
                   let cz = cz /. (float_of_int
                                     (params.blockdim.x * params.blockdim.y))
                   in
                   if abs_float (cx -. cz) < 0.01 then ()
                   else raise Not_found
                | None -> raise Not_found
              else ()
      in
      Some cx
    with Not_found -> None

  let apply_BankConflicts params (p, pext, m, dv) ido idi ph size read =
    (*let p' = match s with
      | Some s -> (fixParams s) @ p
      | None -> p
    in *)
    (* let _ = Printf.printf "apply_BankConflicts\n%!" in *)
    let (params, metric) = match params with
      | Some x -> x
      | None -> failwith "Can't do CUDA analysis without parameters"
    in
    let b =
    if e_is_unique (p, pext, m, dv) (mk () (EVar idi)) then
      ((* Printf.printf "unique\n%!"; *)
       1)
    else
      (* let _ = Printf.printf "not unique\n%!" in *)
      let open CUDA_Cost in
      (* Uncomment for Presburger bounds - slow but maybe good? *)
    let (lbs, ubs) = bounds p (Poly.of_expr (mk () (EVar idi)))
    in
    (*
    let boundplusx x (k, l) =
      (Format.fprintf Format.std_formatter "%d(%s - tid) + %a\n" k idi Presburger.print [l];
       (L.plus (-1) (L.set idi k (L.const 0))
          (L.plus 1 (L.set "tid" k (L.const 0)) (L.plus x (L.const 1) l))))
    in
    let plus32lbs = List.map (boundplusx 32) lbs in
    let plus63lbs = List.map (boundplusx 63) lbs in
     *)
    let bp =
      (*
      if (*(Presburger.implies p (L.set idi 1 (L.set "tid" (-1) (L.const 0)))) &&
           (Presburger.implies p (L.set idi (-1) (L.set "tid" 1 (L.const 0)))) *)
        (List.exists (fun lb -> not (Presburger.sat (lb::p))) plus63lbs) &&
          (not ((List.exists (fun lb -> not (Presburger.sat (lb::p))) plus32lbs)))
      then
        let x = size / 32 in
        if x = 0 then params.warp_size
        else x
      else
        params.warp_size
       *)
      let is_const (k, l) = L.is_empty L.(l.m) in
      (*let _ = Printf.printf "%d lbs, %d ubs\n" (List.length lbs) (List.length ubs) in*)
      let (lbs, ubs) = (List.filter is_const lbs, List.filter is_const ubs) in
      match (lbs, ubs) with
      | ((1, l)::_, (1, u)::_) ->
         (* Subtracting an upper bound from a lower bound?
          * Yes, remember the constants are to the left of the <= sign. *)
         (l.k - u.k) / params.num_shared_banks
      | _ -> ((* Printf.printf "no constant bounds for %s\n" idi; *)
              params.warp_size)
    in
    (* End Presburger *)
    let values_of_x = min params.warp_size params.blockdim.x in
    let values_of_y = min (params.warp_size /^ params.blockdim.x)
                        params.blockdim.y
    in
    let values_of_z = min (params.warp_size /^
                             (params.blockdim.x * params.blockdim.y))
                        params.blockdim.z
    in


    let bd1 =
      let p = get_cd m idi in
      match mult_of_tid params (cfst p) with
      | Some f -> let x = (int_of_float (ceil f)) * size / 32
                  in
                  if x = 0 then params.warp_size
                  else
                    min params.warp_size x
      | None -> params.warp_size
    in
    let bd2 =
      let (d, _, _) = get_cd m idi in
      try
        let (px, py, pz) =
          (find_poly (Some X) d,
           find_poly (Some Y) d,
           find_poly (Some Z) d)
        in
        (* let _ = Printf.printf "valx: %d, valy: %d, valz: %d\n"
                  values_of_x values_of_y values_of_z
        in *)
        let rec gcd a b =
          if b > a then gcd b a
          else if b = 0 then a
          else gcd b (a mod b)
        in
        let values m a n = min m (n / (gcd a n))
        in
        match
          (Poly.compare px (Poly.zero ()) <> 0,
           Poly.compare py (Poly.zero ()) <> 0,
           Poly.compare pz (Poly.zero ()) <> 0)
        with
        | (true, false, false) ->
           (match Poly.is_const px with
            | Some c -> values values_of_x ((int_of_float c) * size / 32)
                          params.num_shared_banks
            | None -> 1)
        | (false, true, false) ->
           (match Poly.is_const py with
            | Some c -> values values_of_y ((int_of_float c) * size / 32)
                          params.num_shared_banks
            | None -> 1)
        | (false, false, true) ->
           (match Poly.is_const pz with
            | Some c -> values values_of_z ((int_of_float c) * size / 32)
                          params.num_shared_banks
            | None -> 1)
        | _ -> 1
      with Not_found -> 1
    in
    let bd2 = params.warp_size / bd2 in
    (* let _ = Printf.printf "bd1: %d, bd2: %d, bp: %d\n" bd1 bd2 bp in *)
    min (min bd1 bd2) bp
    (* min bd1 bd2 *)
    in
    (* let _ = Printf.printf "conflicts: %d\n%!" b in *)
    let b = metric (if read then CUDA_Cost.KArrRead (Shared, b)
                    else CUDA_Cost.KArrAssign (Shared, b))
    in
    (* let _ = Printf.printf "filling in %s\n" idi in *)
    let _ = ph := Some b in
    (* let _ = known_temps := M.add ido b !known_temps in *)
    (p, pext, m, dv)
  (* ((L.set ido (1) (L.const (-b)))::p, m) *)

  let div params (p, pext, m, dv) id o1 o2 =
    let (params, metric) = match params with
      | Some x -> x
      | None -> failwith "Can't do CUDA analysis without parameters"
    in
    try
      let (d1, da1, u1) = get_cd m o1 in
      let (d2, da2, u2) = get_cd m o2 in
      let (px, py, pz, pc) =
        (find_poly (Some X) d1,
         find_poly (Some Y) d1,
         find_poly (Some Z) d1,
         find_poly None d1)
      in
      let d =
        match (dom_is_const (d2, da2, u1), dom_is_param (d2, da2, u2)) with
        | (None, None) -> if cuda_is_unif (d1, da1, u1)
                             && cuda_is_unif (d2, da2, u2)
                          then
                            (cfst cuda_unif, da1 || da2, true)
                          else
                            (cfst cuda_div, da1 || da2, false)
        | (Some None, _) -> if cuda_is_unif (d1, da1, u1) then
                              (cfst cuda_unif, da1 || da2, true)
                            else (cfst cuda_div, da1 || da2, false)
        | (Some Some c, _) ->
           (DM.add (Some X) (V (Poly.mul px (Poly.const (1. /. c))))
             (DM.add (Some Y) (V (Poly.mul py (Poly.const (1. /. c))))
                (DM.add (Some Z) (V (Poly.mul pz (Poly.const (1. /. c))))
                   (DM.add None (V (Poly.mul pc (Poly.const (1. /. c))))
                      DM.empty))),
            da1 || da2,
            u1 && u2)
        | (None, Some (ThreadIdx X, c)) ->
           (DM.add (Some X) (V (Poly.zero ()))
              (DM.add None (V (Poly.mul px (Poly.const (1. /. c)))) d1),
            da1 || da2,
            false)
        | (None, Some (ThreadIdx Y, c)) ->
           (DM.add (Some Y) (V (Poly.zero ()))
             (DM.add None (V (Poly.mul py (Poly.const (1. /. c)))) d1),
            da1 || da2,
            false)
        | (None, Some (ThreadIdx Z, c)) ->
           (DM.add (Some Z) (V (Poly.zero ()))
             (DM.add None (V (Poly.mul pz (Poly.const (1. /. c)))) d1),
            da1 || da2,
            false)
        | (None, Some (WarpSize, c)) ->
           (match mult_of_tid params d1 with
            | Some c1 -> (DM.add None (V (Poly.const (c1 /. c)))
                            (cfst cuda_unif),
                          da1 || da2,
                          true)
            | None -> if cuda_is_unif (d1, da1, u1)
                         && cuda_is_unif (d2, da2, u2) then
                            (cfst cuda_unif, da1 || da2, true)
                          else
                            (cfst cuda_div, da1 || da2, false))
        | _ -> failwith "shouldn't happen"
      in (p, pext, M.add id d m, dv)
    with _ -> (p, pext, M.add id cuda_div m, dv)

  let mod_f pm (p, pext, m, dv) id o1 o2 =
    let (params, metric) = match pm with
      | Some x -> x
      | None -> failwith "Can't do CUDA analysis without parameters"
    in
    let (p', pext') =
      (try
         let (d2, da2, u2) = get_cd m o2 in
         match dom_is_const (d2, da2, u2) with
         | Some Some c ->
            ((* Printf.printf "%s = mod by %f\n" id c; *)
             Presburger.meet [Presburger.L.set id (-1) (Presburger.L.const 0);
                              Presburger.L.set id 1
                                (Presburger.L.const (0 - (int_of_float c)))]
               p,
             Presburger.meet [Presburger.L.set id (-1) (Presburger.L.const 0);
                              Presburger.L.set id 1
                                (Presburger.L.const (0 - (int_of_float c)))]
               pext
            )
         | _ -> (p, pext)
       with _ -> (p, pext))
     in
     (p, pext
      ,
      (M.add id
         (if List.for_all (fun id -> cuda_is_unif (get_cd m id)) [o1; o2] then
            cuda_unif
          else cuda_div)
         m),
    dv)
  let apply_TCallUninterp pm (p, pext, m, dv) id f ids =
    if is_bottom (p, pext, m, dv) then bottom
    else if String.compare f "__div" = 0 then
      match ids with
      | [o1; o2] -> div pm (p, pext, m, dv) id o1 o2
      | _ -> failwith "div must have exactly two arguments"
    else if String.compare f "__mod" = 0 then
      match ids with
      | [o1; o2] -> mod_f pm (p, pext, m, dv) id o1 o2
      | _ -> failwith "mod must have exactly two arguments"
    else
      ( ((*if String.compare f CUDA_Config.memreads_func = 0 then
         match ids with
         | [idi; size; mem; read] ->
            apply_MemReads pm (p, m) id idi (int_of_string size) mem
              (String.compare read "r" = 0)
         | _ -> failwith "memreads must have exactly four arguments"
       else if String.compare f CUDA_Config.bankconflicts_func = 0 then
         match ids with
         | [idi; size; read] ->
            apply_BankConflicts pm (p, m) id idi (int_of_string size)
              (String.compare read "r" = 0)
         | _ -> failwith "conflicts must have exactly three arguments"
        else *)
         p)
      ,
        ((*if String.compare f CUDA_Config.memreads_func = 0 then
         match ids with
         | [idi; size; mem; read] ->
            apply_MemReads pm (pext, m) id idi (int_of_string size) mem
              (String.compare read "r" = 0)
         | _ -> failwith "memreads must have exactly four arguments"
       else if String.compare f CUDA_Config.bankconflicts_func = 0 then
         match ids with
         | [idi; size; read] ->
            apply_BankConflicts pm (pext, m) id idi (int_of_string size)
              (String.compare read "r" = 0)
         | _ -> failwith "conflicts must have exactly three arguments"
        else *)
         pext)
      ,
        (M.add id
           (if List.for_all (fun id -> cuda_is_unif (get_cd m id)) ids then
              cuda_unif
            else cuda_div)
          m)
      , dv)

  let apply (f: ?e:annot Types.expr ref -> Types.id * int -> absval -> unit)
        pm graph hedge tabs =
    (* let _ = Printf.printf "apply %d\n%!" hedge in *)
    let gs = (PSHGraph.info graph).HyperGraph.globs in
    let pred = (PSHGraph.predvertex graph hedge).(0) in
    (* let _ = f pred tabs.(0) in *)
    let transfer = PSHGraph.attrhedge graph hedge in
    let res =
      match transfer with
      | TGuard (disj, log) -> apply_TGuard tabs.(0) (disj, log)
      | TNotUnique lr -> apply_TNotUnique tabs.(0) lr
      | TUnique _ -> tabs.(0)
      | TAssign (id, pe, e, pr) -> apply_TAssign f pm tabs.(0) id (pe, e, pr)
      | TCall _ -> apply_TCall gs tabs.(0)
      | TAddMemReads (ido, idi, _, ph, size, host, read) ->
         let abs = apply_MemReads pm tabs.(0) ido idi ph size host read in
         abs (*
         (match !ph with
          | None -> failwith "apply: should have been filled in"
          | Some n ->
             let e = EAdd (ENum n, EVar ido) in
             apply_TAssign pm abs ido (Some (Poly.of_expr e), ref e)) *)
      | TAddConflicts (ido, idi, _, ph, size, read) ->
         let abs = apply_BankConflicts pm tabs.(0) ido idi ph size read in
         abs (*
         (match !ph with
          | None -> failwith "apply: should have been filled in"
          | Some n ->
             let e = EAdd (ENum n, EVar ido,) in
             apply_TAssign pm abs ido (Some (Poly.of_expr e), ref e)) *)
      | TReturn (f, _) -> apply_TReturn gs tabs.(0) tabs.(1) f
      | TWeaken | TNone -> tabs.(0)
      | TUnify -> let (p, pe, m, _) = tabs.(0) in (p, pe, m, false)
      | TSwitch ids ->
         let (p, pe, m, d) = tabs.(0) in
         (p, pe,
          List.fold_left (fun m id ->
              if M.mem id m then
                M.add id cuda_div m
              else m)
            m
            ids,
          d)
      | TProb p -> tabs.(0)
      | TCallUninterp (id, f, ids) -> apply_TCallUninterp pm tabs.(0) id f ids
    in ((), res)

  let widening (a, ae, m1, dv1) (b, be, m2, dv2) =
    let in_a x = List.exists (L.eq x) a in
    let in_ae x = List.exists (L.eq x) ae in
    (Presburger.minimize (List.filter in_a b),
     Presburger.minimize (List.filter in_ae be),
     M.merge (fun id d1 d2 ->
       let d1 = match d1 with Some x -> x | None -> cuda_bot in
       let d2 = match d2 with Some x -> x | None -> cuda_bot in
       Some (cuda_join id d1 d2))
       m1 m2
    , dv1 || dv2)

  let compute (f: ?e:annot Types.expr ref -> Types.id * int -> absval -> unit) pm graph fstart =
    let info = PSHGraph.info graph in
    let fs = Hashtbl.find info.HyperGraph.funch fstart in
    let _ = args := fs.fun_args in
    let starts =
      PSette.singleton
        PSHGraph.stdcompare.PSHGraph.comparev
        (fstart, fs.fun_body.g_start) in
    let fpman = make_fpmanager graph widening (apply f pm) in
    ( fpman
    , Fixpoint.analysis_std
        fpman graph starts
        (Fixpoint.make_strategy_default
          ~vertex_dummy:("", -1)
          ~hedge_dummy:(-1)
          graph starts) )

end

let debug_print fmt info graph res =
  let print_transfer fmt = function
    | TNone -> ()
    | TUnify -> Format.fprintf fmt "Unify";
    | TSwitch _ -> Format.fprintf fmt "Switch";
    | TWeaken -> Format.fprintf fmt "Weaken";
    | TCall _ -> Format.fprintf fmt "Call";
    | TReturn _ -> Format.fprintf fmt "Return";
    | TGuard (disj, _) ->
      Format.fprintf fmt "Guard %a"
        (Print.list ~first:"(@[<h>" ~sep:" ||@ " ~last:"@])"
          Presburger.print)
        disj;
    | TUnique lr ->
      Format.fprintf fmt "Unique %s" (Utils.logic_to_str (!lr));
    | TNotUnique lr ->
      Format.fprintf fmt "NotUnique %s" (Utils.logic_to_str (!lr));
    | TProb p -> Format.fprintf fmt "Probabilistic branching p = %f" p;
    | TAssign (v, None, _, _) ->
      Format.fprintf fmt "Assign %s = random" v;
    | TAssign (v, Some pe, _, br) ->
       Format.fprintf fmt "Assign[%s] %s = %a"
         (if !br then "P" else "D") v Poly.print_ascii pe;
    | TAddMemReads (ido, idi, _, _, size, host, read) ->
       Format.fprintf fmt "Assign %s = %s + mem%ss(%s, %s, %d)"
         ido ido (if read then "read" else "write") idi
         (if host then "Host" else "Global")
         size
    | TAddConflicts (ido, idi, _, _, size, read) ->
       Format.fprintf fmt "Assign %s = %s + %sconflicts(%s, %d)"
         ido ido (if read then "read" else "write") idi size
    | TCallUninterp (id, f, ids) ->
       Format.fprintf fmt "Assign %s = %s(%a)" id f
         (Print.list ~sep:", " (fun fmt id -> Format.fprintf fmt
                                                "%s" id))
         ids
  in
  let print_hedge_attr fmt hedge transfer =
    Format.fprintf fmt "%d: %a" hedge print_transfer transfer
  in
  PSHGraph.print_dot
    begin fun fmt (vf, vn) -> Format.fprintf fmt "%s_%d" vf vn end
    begin fun fmt hid -> Format.fprintf fmt "e_%d" hid end
    begin fun fmt v _ ->
      Format.fprintf fmt "%d: %a" (snd v) print (PSHGraph.attrvertex res v)
    end
    print_hedge_attr
    fmt graph

(* Common API for abstract interpretation modules. *)

let analyze ?f:(f : ?e:annot Types.expr ref -> Types.id * int -> absval -> unit = fun ?e _ _ -> ()) ~dump pm (gl, fl) fstart =
  let _ = Printf.printf "analyzing with allow_extend=%b\n%!" (!glob_allow_extend) in
  let graph = HyperGraph.from_program (gl, fl) in
  let _ = Printf.printf "got graph\n%!" in
  let info = PSHGraph.info graph in
  (* let _ = Printf.printf "got info\n%!" in *)
  let (fpman, res) = Solver.compute f pm graph fstart in
  let _ = Printf.printf "solved\n%!" in
  (* let _ = PSHGraph.iter_vertex graph
            (fun v abs ~pred ~succ ->
              f v (PSHGraph.attrvertex res v)
            )
  in
   *)
  let _ = PSHGraph.iter_hedge graph
            (fun _ transfer ~pred ~succ ->
              match transfer with
              | TAssign (_, _, e, _) ->
                 let abs = PSHGraph.attrvertex res pred.(0) in
                 f ~e:e pred.(0) abs
              | TAddMemReads (_, id, ann, _, _, _, _)
                | TAddConflicts (_, id, ann, _, _, _) ->
                 let abs = PSHGraph.attrvertex res pred.(0) in
                 f ~e:(ref (mk ann (EVar id))) pred.(0) abs
              | _ -> ()
            )
  in
  if dump then begin
    let fn = "simple_ai.dot" in 
    let oc = open_out fn in
    (*
    let (fn, oc) = Filename.open_temp_file "" ".dot" in
    let fpng = fn ^ ".png" in
    *)
    let fmt = Format.formatter_of_out_channel oc in
    debug_print fmt info graph res;
    close_out oc;
    let cmd = Printf.sprintf "dot -O -Tpng %s" fn in
    if (try Sys.command cmd with Sys_error _ -> 1) <> 0 then
      Printf.eprintf "Error: '%s' failed, be sure to have Graphviz installed.\n" cmd
    (*
    else Utils.show_remove_png fpng;
    Sys.remove fn;
    *)
  end;

  let resh = Hashtbl.create 51 in
  Hashtbl.iter begin fun fname f ->
    Hashtbl.add resh fname
      (Array.make (Array.length f.fun_body.g_edges) top);
  end info.HyperGraph.funch;

  PSHGraph.iter_vertex res
  begin fun (vf, vn) abs ~pred ~succ ->
    let map = Hashtbl.find resh vf in
    map.(vn) <- abs;
  end;
  resh

let get_nonneg (abs, _, _, _) =
  let neg x = float_of_int (-x) in
  let poly_of_linear l =
    L.fold
      (fun v k -> Poly.add_monom (Monom.of_var v) (neg k))
      l.L.m (Poly.const (neg l.L.k))
  in List.map poly_of_linear abs

let print_as_coq varname fmt (av, _, _, _) = Presburger.print_as_coq varname fmt av

let is_bot av = is_bottom av
