(* Quentin Carbonneaux - 2016-2017 *)

type 'a t = 'a list list

let lift x = [[x]]

let true_ = [[]]
let false_ = []
let is_true a = List.mem [] a
let is_false a = a = []

let disjunct = List.rev_append

let rec conjunct a b =
  match a with
  | [] -> []
  | x :: a ->
    disjunct (List.map ((@) x) b) (conjunct a b)

let of_logic ground l =
  let open Types in
  let rec tpos = function
    | LTrue | LRandom -> true_
    | LFalse -> false_
    | LCmp (e1, c, e2) -> ground e1 c e2
    | LAnd (l1, l2) -> conjunct (tpos l1) (tpos l2)
    | LOr (l1, l2) -> disjunct (tpos l1) (tpos l2)
    | LNot l -> tneg l
  and tneg = function
    | LFalse | LRandom -> true_
    | LTrue -> false_
    | LCmp (e1, c, e2) -> ground e1 (Utils.negate_cmp c) e2
    | LOr (l1, l2) -> conjunct (tneg l1) (tneg l2)
    | LAnd (l1, l2) -> disjunct (tneg l1) (tneg l2)
    | LNot l -> tpos l
  in
  tpos l
