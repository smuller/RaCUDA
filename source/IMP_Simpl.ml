(* Stefan Muller - 2019 *)

(* In which we undo some of the dumb things we did to convert CUDA to IMP *)

open Types
open IMP_Types

module IdMap = Map.Make(Id)

let p_none = { pos_file = "";
               pos_line = 0;
               pos_bol = 0;
               pos_char = 0 }

let rec sub_expr dict e =
  match e with
  | EVar v ->
     (try IdMap.find v dict
      with Not_found -> e)
  | EAdd (e1, e2) -> EAdd (sub_expr dict e1, sub_expr dict e2)
  | ESub (e1, e2) -> ESub (sub_expr dict e1, sub_expr dict e2)
  | EMul (e1, e2) -> EMul (sub_expr dict e1, sub_expr dict e2)
  | _ -> e

let rec sub_logic dict l =
  match l with
  | LCmp (e1, c, e2) -> LCmp (sub_expr dict e1, c, sub_expr dict e2)
  | LAnd (l1, l2) -> LAnd (sub_logic dict l1, sub_logic dict l2)
  | LOr (l1, l2) -> LOr (sub_logic dict l1, sub_logic dict l2)
  | LNot l -> LNot (sub_logic dict l)
  | _ -> l

let is_temp v =
  Str.string_match (Str.regexp_string "__temp") v 0

let rec simpl_instr (dict, cost, is) ip =
  let (i, p) = ip in
  match i with
  | IBreak -> (dict, 0, ip::(ITick cost, p)::is)
  | IWeaken
    | IProbIf _
    | IReturn _ -> (dict, cost, ip::is)
  | IAssume l -> (dict, cost, (IAssume (sub_logic dict l), p)::is)
  | IAssign (id, e) ->
     let e' = sub_expr dict e in
     if is_temp id then
       (* Propagate the assignment. It might now be dead code, but if it is,
        * we'll remove it later. *)
       (IdMap.add id e' dict, cost, (IAssign (id, e'), p)::is)
     else
       (IdMap.empty, cost, (IAssign (id, e'), p)::is)
  | IIf (l, b1, b2) ->
     let b1' = simpl_block dict b1 in
     let b2' = simpl_block dict b2 in
     let l' = sub_logic dict l in
     (* Starting a new basic block, so add a tick here *)
     (IdMap.empty, 0, (IIf (l', b1', b2'), p)::(ITick cost, p)::is)
  | IIfNoElse (l, b) ->
     let b' = simpl_block dict b in
     let l' = sub_logic dict l in
     (IdMap.empty, 0, (IIfNoElse (l', b'), p)::(ITick cost, p)::is)
  | IWhile (l, b) ->
     let l' = sub_logic dict l in
     let b' = simpl_block dict b in
     (IdMap.empty, 0, (IWhile (l', b'), p)::(ITick cost, p)::is)
  | ILoop b ->
     let b' = simpl_block dict b in
     (IdMap.empty, 0, (ILoop b', p)::(ITick cost, p)::is)
  | ICall _
    | ICallArg _
    | ICallUninterp _ ->
     (IdMap.empty, 0, ip::(ITick cost, p)::is)
  (* | ITick n -> (dict, cost + n, is) *)
  | ITick n -> (dict, cost + n, is)
  | ITickMemReads (id, bits, host, read) ->
     (match IdMap.find_opt id dict with
      | Some (EVar id') ->
         (dict, cost, (ITickMemReads (id', bits, host, read), p)::is)
      | _ -> (dict, cost, ip::is))
  | ITickConflicts (id, bits, read) ->
     (match IdMap.find_opt id dict with
      | Some (EVar id') ->
         (dict, cost, (ITickConflicts (id', bits, read), p)::is)
      | _ -> (dict, cost, ip::is))
and simpl_block dict b =
  {
    b with b_body =
             let (_, cost, f) =
               List.fold_left simpl_instr (dict, 0, []) b.b_body
             in List.rev ((ITick cost, p_none)::f)
  }

let rec used_vars_expr e =
  match e with
  | EVar id -> IdSet.singleton id
  | EAdd (e1, e2)
    | ESub (e1, e2)
    | EMul (e1, e2) -> IdSet.union (used_vars_expr e1) (used_vars_expr e2)
  | _ -> IdSet.empty

let rec used_vars_logic l =
  match l with
  | LCmp (e1, _, e2) -> IdSet.union (used_vars_expr e1) (used_vars_expr e2)
  | LAnd (l1, l2) -> IdSet.union (used_vars_logic l1) (used_vars_logic l2)
  | _ -> IdSet.empty

let rec used_vars_instr s (i, _) =
  match i with
  | IAssume l -> IdSet.union s (used_vars_logic l)
  | IAssign (_, e) -> IdSet.union s (used_vars_expr e)
  | IIf (l, b1, b2) -> IdSet.union s
                         (IdSet.union (used_vars_logic l)
                            (IdSet.union (used_vars_block b1)
                               (used_vars_block b2)))
  | IIfNoElse (l, b) -> IdSet.union s
                          (IdSet.union (used_vars_logic l)
                             (used_vars_block b))
  | IWhile (l, b) -> IdSet.union s (IdSet.union (used_vars_logic l)
                                      (used_vars_block b))
  | ILoop b -> IdSet.union s (used_vars_block b)
  | ICall id -> IdSet.add id s
  (* | ITick n -> IdSet.union s (used_vars_expr e) *)
  | ICallArg (rets, f, args) ->
      List.fold_left (fun s e -> IdSet.add e s) s (f::(rets @ args))
  | ICallUninterp (ret, f, args) ->
      List.fold_left (fun s e -> IdSet.add e s) s (f::ret::args)
  | IReturn ids ->
     List.fold_left (fun s e -> IdSet.add e s) s ids
  | ITickMemReads (id, _, _, _)
    | ITickConflicts (id, _, _) ->
     IdSet.add id s
  | _ -> s

and used_vars_block b =
  List.fold_left used_vars_instr IdSet.empty b.b_body

let live_code used_vars (i, _) =
  match i with
  | IAssign (id, _) -> IdSet.mem id used_vars
  | ITick 0 -> false
  | _ -> true

let rec dce_instr used (i, p) =
  (match i with
   | IIf (l, b1, b2) -> IIf (l, dce_block used b1, dce_block used b2)
   | IIfNoElse (l, b) -> IIfNoElse (l, dce_block used b)
   | IWhile (l, b) -> IWhile (l, dce_block used b)
   | ILoop b -> ILoop (dce_block used b)
   | _ -> i)
  , p
and dce_block used b =
  { b with b_body = List.map (dce_instr used)
                      (List.filter (live_code used) b.b_body) }

let simpl_func f =
  { f with fun_body =
             let b = f.fun_body in
             let b' = simpl_block IdMap.empty b in
             let used = used_vars_block b' in
             dce_block used b'
  }

let simple_prog (globals, funcs) =
  (globals, List.map simpl_func funcs)
