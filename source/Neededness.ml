(* Anlun Xu - 2018 *)
(* Van Chan Ngo - 2018 *)

let p_compare = compare

open Types
open Printf
open Graph

exception Node_does_not_exist

module NodeVar = 
  struct
    type t = node * id
    let compare (n, v) (n', v') = 
      match p_compare n n' with
          0 -> p_compare v v'
        | c -> c
  end
module Int = 
  struct
    type t = int 
    let compare = Int.compare
  end 
module NeededSet = Set.Make(NodeVar)
module NodeSet = Set.Make(Int)
module VarSet = Set.Make(String)

(* The map that maps from node to a set of its needed variables *)
module NeededMap = Map.Make(Int)

(* Analyze the graph of a function f, return a set of needed variable *)
let analyze f = 

  (* traverse expression, add variable into a var set *)
  let rec traverse_exp exp vars = 
    match exp with
    | EVar id -> VarSet.add id vars
    | EAdd (exp1, exp2) -> traverse_exp exp1 (traverse_exp exp2 vars)
    | ESub (exp1, exp2) -> traverse_exp exp1 (traverse_exp exp2 vars)
    | EMul (exp1, exp2) -> traverse_exp exp1 (traverse_exp exp2 vars)
    | ERandom | ENum _ | EBer _ | EBin _ | EGeo _ 
    | ENbin _ | EPois _ | EUnif _ | EHyper _
    | EDist _ -> vars
     
  in

  (* traverse the logic, add variable into a var set *)
  let rec traverse_logic logic vars = 
    match logic with 
    | LCmp (exp1, _, exp2) -> 
      traverse_exp exp1 (traverse_exp exp2 vars)
    | LAnd (logic1, logic2) -> 
      traverse_logic logic1 (traverse_logic logic2 vars)
    | LOr (logic1, logic2) -> 
      traverse_logic logic1 (traverse_logic logic2 vars)
    | LNot logic' -> 
      traverse_logic logic' vars 
    | LTrue | LFalse | LRandom -> vars
  in 

  (* Insert necessary variables of a node into the neededness map *)
  let update_nec node needed (action, _) =
    match action with
    | AGuard logic -> 
      let old_var_set = NeededMap.find node needed in
      let empty = VarSet.empty in
      let new_neededness = traverse_logic logic empty in
      let new_var_set = VarSet.union old_var_set new_neededness in
      NeededMap.add node new_var_set needed
    | AAssign (id, e, _) -> 
        if id = "tick_z" then  
          let old_var_set = NeededMap.find node needed in
          let empty = VarSet.empty in 
          let new_neededness = traverse_exp !e empty in
          let new_var_set = VarSet.union old_var_set new_neededness in 
          NeededMap.add node new_var_set needed
        else needed
    | ANone | AUnify | AWeaken | ACall _ | AProb _ | AUnique _ | ANotUnique _
      | AAddMemReads _ | AAddConflicts _ | ACallUninterp _ | ASwitch _ -> needed
  in

  (* Compute the necessary variable in current node *)
  let necessary node needed = 
    (* For each edge *)
    let edges = f.g_edges.(node) in 
    List.fold_left (update_nec node) needed edges 
  in

  (* Find the preceding nodes of current node *)
  let find_parents node =
    let find_parents' parents edge_list = 
      List.fold_left 
        (fun parents -> fun (parent, action, child) -> 
          if child = node then NodeSet.add parent parents else parents)
        parents edge_list
    in
    (* map edges into (parent, action, child) form *)
    let edges = 
      Array.mapi 
      (fun i -> fun edge_list -> 
        List.map (fun (action, node) -> (i, action, node)) edge_list) 
      f.g_edges
    in
    Array.fold_left (fun parents -> fun edge_list -> find_parents' parents edge_list) 
      NodeSet.empty edges 
  in

  (* return true if var is defined at node *) 
  let def node var = 
    let edges = f.g_edges.(node) in 
    let check_def (action, node) = 
      match action with 
      | AAssign(id, exp, _) -> id = var
      | ANone | AUnify | ASwitch _ | AWeaken | AGuard _ | ACall _ | AProb _ | AUnique _ | ANotUnique _ 
        | AAddMemReads _ | AAddConflicts _ | ACallUninterp _ -> false
    in
    List.fold_left (||) false (List.map check_def edges) 
  in
      
  (* return a set of variables used to define variable var at node *)
  let used_var var node = 
    let vars = VarSet.empty in
    let edges = f.g_edges.(node) in 
    let add_vars vars (action, _) = 
      match action with 
      | AAssign (id, exp, _) -> 
        (match id = var with 
         | true -> traverse_exp !exp vars
         | false -> vars)
      | ANone | AUnify | ASwitch _ | AWeaken | AGuard _ | ACall _ | AProb _ | AUnique _ | ANotUnique _ 
      | AAddMemReads _ | AAddConflicts _ | ACallUninterp _ -> vars
    in
    List.fold_left add_vars vars edges 
  in

  (* Propogate the needeness backward *)
  let rec propogate node needed = 
    let parents = find_parents node in 
    let propogate_to parent needed =
      let needed_vars = 
        NeededMap.find node needed
      in
      let n2 = 
        VarSet.filter (fun var -> not(def parent var)) needed_vars
      in
      let n3 = 
        let defined = 
          VarSet.filter (fun var -> def parent var) needed_vars
        in 
          VarSet.fold 
          (fun var -> fun needed -> VarSet.union needed (used_var var parent))
          defined
          VarSet.empty
      in
      let old_neededness = NeededMap.find parent needed in
      let new_neededness = VarSet.union n2 n3 in
      if VarSet.subset new_neededness old_neededness then
        needed
      else 
        let neededness = VarSet.union old_neededness new_neededness in
        let new_needed_map = NeededMap.add parent neededness needed in
        propogate parent new_needed_map
    in
    NodeSet.fold 
    (fun parent -> fun needed -> propogate_to parent needed) 
    parents 
    needed 
  in

  let empty_map = NeededMap.empty in
  let index = 
    Array.mapi (fun i -> fun edge_list -> i) f.g_edges 
  in
  let initial_map =  
    Array.fold_left 
    (fun needed -> fun i -> NeededMap.add i (VarSet.empty) needed)
    empty_map
    index 
  in
  Array.fold_left 
  (fun needed -> fun i -> propogate i (necessary i needed)) 
  initial_map
  index 

(* Given the CFG and node, return a list of needed variable at that node *)
let needed_variables f = 
  let f_graph = f.fun_body in
  let needed_map = analyze f_graph in
  begin
    fun node -> 
      if node >= Array.length f_graph.g_edges || node < 0 then
        raise Node_does_not_exist
      else
        let needed = NeededMap.find node needed_map in
        VarSet.elements needed
  end
