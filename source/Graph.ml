(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types
open Dist
include Graph_Types

type stats =
  { mutable weaken_map: (id * int) list }

let stats =
  { weaken_map = [] }

(* print a graph for testing 
 * globals: set of global variables
 * func_l: list of functions whose bodies are graphs
 *)
let rec id_lst_to_str l = 
  let rec aux l str no_comma = 
    match l with
    | [] -> str
    | h :: t -> 
      let comma = if no_comma then "" else "," in 
      aux t (str ^ comma ^ (Printf.sprintf "%s" h)) false
  in
  aux l "" true

let action_to_str act = 
  match act with
  | ANone -> 
    "none"
  | AWeaken ->
    "weaken"
  | AGuard lg ->
     Utils.logic_to_str lg
  | AUnique lg ->
     ("U:" ^ (Utils.logic_to_str (!lg)))
  | ANotUnique lg ->
     ("NU:" ^ (Utils.logic_to_str (!lg)))
  | AAssign (id, e, br) ->
     (Printf.sprintf "%s =[%s] " (if !br then "P" else "D") id) ^
       (Utils.expr_to_str (!e))
  | AAddMemReads (ido, idi, _, size, host, read) ->
       Printf.sprintf "%s = %s + mem%ss(%s, %s, %d)"
         ido ido (if read then "read" else "write") idi
         (if host then "Host" else "Global")
         size
  | AAddConflicts (ido, idi, _, size, read) ->
       Printf.sprintf "%s = %s + %sconflicts(%s, %d)"
         ido ido (if read then "read" else "write") idi size
  | ACall id | ACallUninterp (id, _, _) ->
    Printf.sprintf "call %s()" id 
  | AProb prob ->
     Printf.sprintf "prob %f" prob

let print_graph_edge oc src (act, dst) =
  Printf.fprintf oc "(%d -- %s --> %d)\n" src (action_to_str act) dst

let print_graph_edges_from_src oc neededness src edgs = 
  match neededness with
  | None -> 
    List.iter (print_graph_edge oc src) edgs
  | Some a -> 
    List.iter (print_graph_edge oc src) edgs;
    Printf.fprintf oc "[needed vars: %s]\n" (id_lst_to_str (a src))

let print_graph_f oc neededness f = 
  Printf.fprintf oc "function %s(" f.fun_name;
  let args_to_str = id_lst_to_str f.fun_args in 
  Printf.fprintf oc "%s) " args_to_str;
  Printf.fprintf oc "local(";
  let locs_to_str = id_lst_to_str f.fun_vars in 
  Printf.fprintf oc "%s)\n" locs_to_str;
  let graph_body = f.fun_body in
  Printf.fprintf oc "start node: %d\n" graph_body.g_start;
  Printf.fprintf oc "end node: %d\n" graph_body.g_end;
  Array.iteri (print_graph_edges_from_src oc neededness) graph_body.g_edges;
  Printf.fprintf oc "\n"

let print_graph_prg ?need_analysis:(neededness = None) oc globals func_l =
  (* print global variables *)
  Printf.fprintf oc "global(";
  let globals_to_str = id_lst_to_str globals in 
  Printf.fprintf oc "%s)\n" globals_to_str;
  Printf.fprintf oc "\n";
  (* print all functions *)
  List.iter (print_graph_f oc neededness) func_l

(* Construct a graph representation of the
   given IMP program.  The nodes of the graph
   are program points and the edges have
   actions on them.
 *)

type annot = (unit CUDA_Types.cexpr * unit CUDA_Types.cexpr) option ref
type hannot = annot Types.expr list

            
let from_imp_with_annots transformed tick_var
      (impf : (Focus.focus, 'annot IMP.block) Types.func_)
    : 'c * ((int, 'annot Types.expr list) Hashtbl.t) =

  let h_position = Hashtbl.create 51 in
  let h_edges = Hashtbl.create 51 in
  let h_annots = Hashtbl.create 51 in

  let next_node = ref 0 in
  let new_node a pos =
    let node = !next_node in
    Hashtbl.add h_position node pos;
    Hashtbl.add h_annots node a;
    incr next_node;
    node
  in

  let new_edge src act dst =
    if dst < 0 then
      failwith "Graph:new_edge: invalid input program";
    Hashtbl.add h_edges src (act, dst)
  in

  let create_assign_edge a id e fin pos = 
    let beg = new_node a pos in
    new_edge beg (AAssign (id, ref e, ref false)) fin; 
    beg
  in
  let create_pot_assign_edge a id e fin pos = 
    let beg = new_node a pos in
    new_edge beg (AAssign (id, ref e, ref true)) fin; 
    beg
  in
  let create_switch_edge a e fin pos =
    let beg = new_node a pos in
    new_edge beg (ASwitch (IMP.written_block e)) fin;
    beg
  in

  let to_prob_branching a id e op fin pos keys probs =
    let new_node = new_node a in
    let last_key = List.hd (List.rev keys) in
    if false then Printf.printf "to_prob_branching last_key = %d\n" last_key;
    let keys' = List.rev (List.tl (List.rev keys)) in
    let beg = new_node pos in

    let create_prob_branching n (k, p) = 
      let ln' = new_node pos in
      let rn' = new_node pos in
      new_edge n (AProb p) ln';
      if false then Printf.printf "create_prob_branching k, p = %d %f\n" k p;
      (* edge to fin *)
      let e' = match op with
               | Dadd ->
                 mk () (EAdd (e, mk () (ENum k)))
               | Dsub ->
                 mk () (ESub (e, mk () (ENum k)))
               | Dmul ->
                 mk () (EMul (e, mk () (ENum k)))
      in
      new_edge ln' (AAssign (id, ref e', ref false)) fin;
      new_edge n (AProb (1. -. p)) rn';
      if false then Printf.printf "create_prob_branching k, 1 - p = %d %f\n" k (1. -. p);
      rn'
    in

    let last_level_node = List.fold_left create_prob_branching beg (List.combine keys' probs) in
    (* the last edge *)
    let e' = match op with
             | Dadd ->
               mk () (EAdd (e, mk () (ENum last_key)))
             | Dsub ->
               mk () (ESub (e, mk () (ENum last_key)))
             | Dmul ->
               mk () (EMul (e, mk () (ENum last_key)))
    in
    new_edge last_level_node (AAssign (id, ref e', ref false)) fin;
    beg
  in

  let transform_ber a id e op d fin pos =
    match desc d with
    | EBer (pa, pb) ->
      let (keys, probs) = List.split (Dist.ber_to_list pa pb) in
      if false then List.iter2 (Printf.printf "(%d : %f)\n") keys probs;
      to_prob_branching a id e op fin pos keys [List.hd probs]
    | _ -> failwith "Graph:ber: invalid input program"
  in
  
  let transform_bin a id e op d fin pos = 
    match desc d with
    | EBin (n, pa, pb) -> 
      let key_probs = Dist.bin_to_list n pa pb in 
      let transformed_probs = Dist.transform_prob key_probs in 
      if false then List.iter2 (Printf.printf "(%d : %f)\n") (fst (List.split key_probs)) (snd (List.split key_probs));
      to_prob_branching a id e op fin pos (fst (List.split key_probs)) transformed_probs
    | _ -> failwith "Graph:bin: invalid input program"
  in

  let transform_geo a id e op d fin pos = 
    failwith "Geometric sampling distribution is not supported"
  in

  let transform_nbin a id e op d fin pos = 
    failwith "Negative binomial sampling distribution is not supported"
  in

  let transform_pois a id e op d fin pos =
    failwith "Poisson sampling distribution is not supported"
  in

  let transform_hyper a id e op d fin pos = 
    match desc d with
    | EHyper (n, r, m) ->
      let key_probs = Dist.hyper_to_list n r m in 
      let transformed_probs = Dist.transform_prob key_probs in
      if false then List.iter2 (Printf.printf "(%d : %f)\n") (fst (List.split key_probs)) (snd (List.split key_probs));
      to_prob_branching a id e op fin pos (fst (List.split key_probs)) transformed_probs
    | _ -> failwith "Graph:hyper: invalid input program"
  in

  let transform_unif a  id e op d fin pos = 
    match desc d with
    | EUnif (lb, ub) ->
      let key_probs = Dist.unif_to_list lb ub in
      let transformed_probs = Dist.transform_prob key_probs in
      if false then List.iter2 (Printf.printf "(%d : %f)\n") (fst (List.split key_probs)) (snd (List.split key_probs));
      to_prob_branching a id e op fin pos (fst (List.split key_probs)) transformed_probs
    | _ -> failwith "Graph:unif: invalid input program"
  in

  let transform_dist a id e fin loo pos =
    match desc e with
    | EAdd (e1, e2) ->
      let dt1 = Dist.get_type e1 in
      let dt2 = Dist.get_type e2 in
      begin
        match (dt1, dt2) with
        | (Det, Det) ->
          create_assign_edge a id e fin pos
        | (Det, Ber) -> 
          transform_ber a id e1 Dadd e2 fin pos
        | (Det, Bin) ->
          transform_bin a id e1 Dadd e2 fin pos
        | (Det, Geo) ->
          transform_geo a id e1 Dadd e2 fin pos
        | (Det, Nbin) ->
          transform_nbin a id e1 Dadd e2 fin pos
        | (Det, Pois) ->
          transform_pois a id e1 Dadd e2 fin pos
        | (Det, Hyper) ->
          transform_hyper a id e1 Dadd e2 fin pos
        | (Det, Unif) ->
          transform_unif a id e1 Dadd e2 fin pos
        | (Ber, Det) -> 
          transform_ber a id e2 Dadd e1 fin pos
        | (Bin, Det) ->
          transform_bin a id e2 Dadd e1 fin pos
        | (Geo, Det) ->
          transform_geo a id e2 Dadd e1 fin pos
        | (Nbin, Det) ->
          transform_nbin a id e2 Dadd e1 fin pos
        | (Pois, Det) ->
          transform_pois a id e2 Dadd e1 fin pos
        | (Hyper, Det) ->
          transform_hyper a id e2 Dadd e1 fin pos
        | (Unif, Det) ->
          transform_unif a id e2 Dadd e1 fin pos
        | (_, _) ->
          failwith "Graph:transform_dist:EAdd: invalid input program"
      end
    | ESub (e1, e2) ->
      let dt1 = Dist.get_type e1 in
      let dt2 = Dist.get_type e2 in
      begin
        match (dt1, dt2) with
        | (Det, Det) ->
          create_assign_edge a id e fin pos
        | (Det, Ber) -> 
          transform_ber a id e1 Dsub e2 fin pos
        | (Det, Bin) ->
          transform_bin a id e1 Dsub e2 fin pos
        | (Det, Geo) ->
          transform_geo a id e1 Dsub e2 fin pos
        | (Det, Nbin) ->
          transform_nbin a id e1 Dsub e2 fin pos
        | (Det, Pois) ->
          transform_pois a id e1 Dsub e2 fin pos
        | (Det, Hyper) ->
          transform_hyper a id e1 Dsub e2 fin pos
        | (Det, Unif) ->
          transform_unif a id e1 Dsub e2 fin pos
        | (Ber, Det) -> 
          transform_ber a id e2 Dsub e1 fin pos
        | (Bin, Det) ->
          transform_bin a id e2 Dsub e1 fin pos
        | (Geo, Det) ->
          transform_geo a id e2 Dsub e1 fin pos
        | (Nbin, Det) ->
          transform_nbin a id e2 Dsub e1 fin pos
        | (Pois, Det) ->
          transform_pois a id e2 Dsub e1 fin pos
        | (Hyper, Det) ->
          transform_hyper a id e2 Dsub e1 fin pos
        | (Unif, Det) ->
          transform_unif a id e2 Dsub e1 fin pos
        | (_, _) ->
          failwith "Graph:transform_dist:ASub: invalid input program"
      end
    | EMul (e1, e2) ->
      let dt1 = Dist.get_type e1 in
      let dt2 = Dist.get_type e2 in
      begin
        match (dt1, dt2) with
        | (Det, Det) ->
          create_assign_edge a id e fin pos
        | (Det, Ber) -> 
          transform_ber a id e1 Dmul e2 fin pos
        | (Det, Bin) ->
          transform_bin a id e1 Dmul e2 fin pos
        | (Det, Geo) ->
          transform_geo a id e1 Dmul e2 fin pos
        | (Det, Nbin) ->
          transform_nbin a id e1 Dmul e2 fin pos
        | (Det, Pois) ->
          transform_pois a id e1 Dmul e2 fin pos
        | (Det, Hyper) ->
          transform_hyper a id e1 Dmul e2 fin pos
        | (Det, Unif) ->
          transform_unif a id e1 Dmul e2 fin pos
        | (Ber, Det) -> 
          transform_ber a id e2 Dmul e1 fin pos
        | (Bin, Det) ->
          transform_bin a id e2 Dmul e1 fin pos
        | (Geo, Det) ->
          transform_geo a id e2 Dmul e1 fin pos
        | (Nbin, Det) ->
          transform_nbin a id e2 Dmul e1 fin pos
        | (Pois, Det) ->
          transform_pois a id e2 Dmul e1 fin pos
        | (Hyper, Det) ->
          transform_hyper a id e2 Dmul e1 fin pos
        | (Unif, Det) ->
          transform_unif a id e2 Dmul e1 fin pos
        | (_, _) ->
          failwith "Graph:transform_dist:EMul: invalid input program"
      end
    | _ ->
      failwith "it should not happen"
  in

  let rec eol l =
    match l with
    | LTrue | LFalse | LRandom -> []
    | LCmp (e1, _, e2) -> [e1; e2]
    | LAnd (l1, l2) | LOr (l1, l2) -> (eol l1) @ (eol l2)
    | LNot l -> eol l
  in

  (* graph for one instruction *)
  let rec goi fin loo pos : 'a IMP_Types.instr -> node =
    (*
    let create_assign_edge = create_assign_edge a in
    let new_node = new_node a in
     *)
    let create_switch_edge = create_switch_edge [] in
    function
    | IMP.IBreak -> 
      loo
    | IMP.IWeaken ->
      let beg = new_node [] pos in
      new_edge beg AWeaken fin;
      beg
    | IMP.IAssume log ->
      let beg = new_node [] pos in
      new_edge beg (AGuard (erase_l log)) fin;
      beg
    | IMP.IAssign (id, e) ->
       (* transform all samplings into equivalent probabilistic branchings *)
       (*
      if transformed then
        begin
           match desc e with
          | ERandom ->
            (*failwith "expression contains random"*)
            create_assign_edge [] id e fin pos
          | EVar _ | ENum _ | EDist _ -> 
            create_assign_edge [] id e fin pos
          | EBer _ | EBin _ | EGeo _ | ENbin _ | EPois _ | EHyper _ | EUnif _ -> 
            transform_dist [] id (mk () EAdd ((ENum 0), e)) fin loo pos
          | EAdd _ | ESub _ | EMul _ -> 
            if (Dist.is_sampling e) then 
              transform_dist [] id e fin loo pos
            else 
              create_assign_edge [e] id e fin pos
        end
      else *)
        create_assign_edge [e] id (erase_e e) fin pos
    | IMP.ITick n ->
       create_pot_assign_edge [] tick_var
         (mk () (EAdd (mk () (EVar tick_var), mk () (ENum n)))) fin pos
    | IMP.ITickMemReads (id, size, host, read) ->
       let beg = new_node [] pos in
       let ph = ref None in
       new_edge beg (AAddMemReads (tick_var, id, ph, size, host, read)) fin;
       beg
    | IMP.ITickConflicts (id, size, read) ->
       let beg = new_node [] pos in
       let ph = ref None in
       new_edge beg (AAddConflicts (tick_var, id, ph, size, read)) fin;
       beg
    | IMP.IIf (log, bi, be) ->
       let a = eol log in
       let beg = new_node a pos in
       let fini = new_node [] be.IMP.b_end_p in
       let fin2a = new_node [] be.IMP.b_end_p in
       let fin2 = create_switch_edge be fin2a be.IMP.b_end_p in
      let begi = gob fini loo bi in
      let bege = gob fin2 loo be in
      let bege2 = gob fini loo be in
      let bege2 = create_switch_edge bi bege2 be.IMP.b_start_p in
      let begi2 = gob bege2 loo bi in
      let tick = create_pot_assign_edge a tick_var
                   (mk () (EAdd (mk () (EVar tick_var),
                          mk () (EVar (CUDA_Config.div_cost)))))
                   begi2
                   pos
      in
      new_edge beg (ANotUnique (ref (erase_l log))) tick;
      (* new_edge fini (ANotUnique (ref log)) tick; *)
      new_edge fini (ANone) fin2a;
      (* XXX new_edge beg (ANotUnique (ref log)) begi2; *)
      new_edge beg (AGuard (erase_l log)) begi;
      new_edge beg (AGuard (erase_l (LNot log))) bege;
      (* new_edge beg (ANone) begi2; *)
      new_edge fin2a AUnify fin;
      beg
    | IMP.IIfNoElse (log, bi) ->
       let beg = new_node (eol log) pos in
       let fini = new_node [] bi.IMP.b_end_p in
       let fin2a = new_node [] bi.IMP.b_end_p in
       let fin2 = create_switch_edge bi fin2a bi.IMP.b_end_p in
      let begi = gob fini loo bi in
      let begi2 = gob fin2 loo bi in
      let tick = create_pot_assign_edge [] tick_var
                   (mk () (EAdd (mk () (EVar tick_var),
                          mk () (EVar (CUDA_Config.div_cost)))))
                   begi2
                   pos
      in
      new_edge beg (ANotUnique (ref (erase_l log))) tick;
      (* new_edge fini (ANotUnique (ref log)) tick; *)
      new_edge fini (ANone) fin2a;
      (* XXX new_edge beg (ANotUnique (ref log)) begi2; *)
      new_edge beg (AGuard (erase_l log)) begi;
      new_edge beg (AGuard (erase_l (LNot log))) fin2a;
      (* new_edge beg (ANone) begi2; *)
      new_edge fin2a AUnify fin;
      beg
       (*
       let beg = new_node pos in
       let fin2 = new_node bi.IMP.b_end_p in
       let begi = gob fin2 loo bi in
       let tick = create_pot_assign_edge tick_var
                   (EAdd (EVar tick_var,
                          EVar (CUDA_Config.div_cost)))
                   fin
                   pos
       in
       new_edge fin2 (ANotUnique (ref log)) tick;
       new_edge beg (AGuard log) begi;
       new_edge beg (AGuard (LNot log)) fin2;
       new_edge fin2 AWeaken fin;
       beg
        *)
    | IMP.IWhile (log, b) ->
       (* not counting divwarps without else *) (*
      let jmp = new_node b.IMP.b_end_p in
      let begb = gob jmp fin b in
      new_edge jmp (AGuard log) begb;
      new_edge jmp (AGuard (LNot log)) fin;
      jmp *)
       (* counting divwarps without else *)
       let fin = create_switch_edge b fin b.IMP.b_end_p in
      let beg = new_node (eol log) pos in
      let jmp = new_node [] b.IMP.b_end_p in
      let begb = gob beg fin b in
      let tick = create_pot_assign_edge [] tick_var
                   (mk () (EAdd (mk () (EVar tick_var),
                          mk () (EVar (CUDA_Config.div_cost)))))
                   begb
                   pos
      in
      new_edge jmp (ANotUnique (ref (erase_l log))) tick;
      new_edge beg ANone jmp;
      new_edge jmp (AGuard (erase_l log)) begb;
      new_edge jmp (AGuard (erase_l (LNot log))) fin;
      beg
    | IMP.ILoop b ->
      let jmp = new_node [] b.IMP.b_end_p in
      let begb = gob jmp fin b in
      new_edge jmp ANone begb;
      begb
    | IMP.ICall idf ->
      let beg = new_node [] pos in
      new_edge beg (ACall idf) fin;
      beg
    | IMP.IProbIf (prob_e, bl, br) -> 
      let beg = new_node [] pos in
      let beg_bl = gob fin loo bl in
      let beg_br = gob fin loo br in
      let p1 = match prob_e with 
               | EProb (a, b) -> (float_of_int a) /. (float_of_int (a + b))
      in
      new_edge beg (AProb p1) beg_bl;
      new_edge beg (AProb (1. -. p1)) beg_br;
      beg
    | IMP.IReturn _ -> failwith "Return command should not happen"
    | IMP.ICallArg (rl, f, argl) -> failwith "Call with arguments should not happen"
    | IMP.ICallUninterp (ret, f, argl) ->
       let beg = new_node [] pos in
       new_edge beg (ACallUninterp (ret, f, argl)) fin;
       beg

  and goil fin loo = function
    | [] -> fin
    | (i, pos) :: b ->
      let fin = goil fin loo b in
      goi fin loo pos i

  and gob fin loo ({ IMP.b_end_p; b_body } : 'a IMP.block) =
    let beg = new_node [] b_end_p in
    new_edge beg ANone fin;
    goil beg loo b_body

  in

  let g_end = new_node [] impf.fun_end_p in
  let g_start = gob g_end (-1) impf.fun_body in
  let g_position = Array.make !next_node impf.fun_end_p in
  let g_edges = Array.make !next_node [] in
  for i = 0 to !next_node - 1 do
    g_position.(i) <- Hashtbl.find h_position i;
    g_edges.(i) <- Hashtbl.find_all h_edges i;
  done;
  ({ impf with fun_body = { g_start; g_end; g_edges; g_position } },
   h_annots)

let from_imp transformed tick_var impf =
  fst (from_imp_with_annots transformed tick_var impf)

let rpo_order gfunc =
  let g = gfunc.fun_body in
  let map = Array.map (fun _ -> -1) g.g_edges in
  let rec dfs node n =
    if map.(node) <> -1 then n else
    begin
      map.(node) <- 0;
      let n = List.fold_left
        (fun n (_, dst) -> dfs dst n)
        n g.g_edges.(node) in
      map.(node) <- n;
      n + 1
    end in
  let nlive = dfs g.g_start 0 in
  Array.iteri (fun i n -> map.(i) <- nlive - 1 - n) map;
  let new_edges = Array.make nlive [] in
  let new_position = Array.make nlive Utils.dummy_position in
  Array.iteri (fun i n ->
    let tr_edges = List.map (fun (a, d) -> (a, map.(d))) in
    if n <> nlive then begin
      new_edges.(n) <- tr_edges g.g_edges.(i);
      new_position.(n) <- g.g_position.(i);
    end
  ) map;
  { gfunc with fun_body =
    { g_start = map.(g.g_start)
    ; g_end = map.(g.g_end)
    ; g_edges = new_edges
    ; g_position = new_position
    }
  }

let add_loop_counter cnt gfunc =
  let gfunc = rpo_order gfunc in
  let g = gfunc.fun_body in
  let nnodes = Array.length g.g_edges in
  let incs = ref [[AAssign (cnt, ref (mk () (ENum 0)), ref false), g.g_start]]
  in
  let nincs = ref 1 in
  let add_increment dst =
    let e = ref (mk () (EAdd (mk () (ENum 1), mk () (EVar cnt)))) in
    incs := [AAssign (cnt, e, ref false), dst] :: !incs;
    incr nincs;
    !nincs - 1 + nnodes
  in
  let new_edges =
    Array.mapi begin fun src ->
      List.map begin function
        | (a, dst) when dst <= src ->
	  (a, add_increment dst)
	| e -> e
      end
    end g.g_edges
  in
  let incs = Array.of_list (List.rev !incs) in
  let new_position =
    Array.map begin function
      | [_, dst] -> g.g_position.(dst)
      | _ -> assert false
    end incs
  in
  { gfunc with
    fun_vars = cnt :: gfunc.fun_vars;
    fun_body = {
      g with
      g_start = nnodes;
      g_edges = Array.append new_edges incs;
      g_position = Array.append g.g_position new_position;
    };
  }

module type AbsInt = sig
  type absval
  val analyze: ?f:(Types.id * int -> absval -> unit) ->
               dump:bool ->
               (CUDA_Cost.array_params * CUDA_Cost.cmetric) option ->
               (id list * func list) -> id ->
               (string, absval array) Hashtbl.t
  val is_nonneg: absval -> Polynom.Poly.t -> bool
  val get_nonneg: absval -> Polynom.Poly.t list
  val is_bot: absval -> bool

  val print_as_coq: (id -> string) -> Format.formatter -> absval -> unit
end

module AbsInt = struct
  (*  module Apron: AbsInt = Graph_AI_Apron *)
  module Simple: AbsInt = Graph_AI_Simple
end
