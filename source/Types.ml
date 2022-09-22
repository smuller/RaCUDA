(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

(* Bernoulli distribution - ber(pa, pb) *)
(* Binomial distribution - bin(n, pa, pb) *)
(* Geometric distribution - geo(pa, pb) *)
(* Negative binomial distribution - nbin(r, pa, pb) *)
(* Poisson distribution - pois(la, lb) *) 
(* Hyper-geometric distribution - hyper(n, r, m) *)
(* Uniform distribution - unif(a, b) *)

type id = string

module Id = String
module IdSet = struct
  include Set.Make(Id)

  let of_list =
    List.fold_left (fun s i -> add i s) empty
end

type position =
  { pos_file: string
  ; pos_line: int
  ; pos_bol: int
  ; pos_char: int
  }

type 'a expr_ =
  | ERandom
  | EVar of id
  | ENum of int
  | EAdd of 'a expr * 'a expr
  | ESub of 'a expr * 'a expr
  | EMul of 'a expr * 'a expr
  | EBer of int * int
  | EBin of int * int * int
  | EGeo of int * int
  | ENbin of int * int * int
  | EPois of int * int
  | EHyper of int * int * int
  | EUnif of int * int
  | EDist of id * int * int * int * int

and 'a expr = 'a * 'a expr_

let desc (a, e) = e
let ann (a, e) = a
let mk (a: 'a) (e: 'a expr_) : 'a expr = (a, e)
            
type prob_expr =
  | EProb of int * int

type 'a free_expr =
  | FBase of 'a expr
  | FApply of id * 'a free_expr list * position

type ('a, 'b) func_ =
  { fun_name: id
  ; fun_vars: id list
  ; fun_focus: 'a list
  ; fun_body: 'b
  ; fun_start_p: position
  ; fun_end_p: position
  ; fun_args: id list
  }

type cmp = Le | Lt | Ge | Gt | Eq | Ne

type 'a logic =
  | LTrue
  | LFalse
  | LRandom
  | LCmp of 'a expr * cmp * 'a expr
  | LAnd of 'a logic * 'a logic
  | LOr of 'a logic * 'a logic
  | LNot of 'a logic

let rec erase_e e =
  mk ()
    (match desc e with
     | EVar x -> EVar x
     | ENum n -> ENum n
     | EAdd (e1, e2) -> EAdd (erase_e e1, erase_e e2)
     | ESub (e1, e2) -> ESub (erase_e e1, erase_e e2)
     | EMul (e1, e2) -> EMul (erase_e e1, erase_e e2)
    )

let rec erase_l l =
  match l with
  | LTrue -> LTrue
  | LFalse -> LFalse
  | LRandom  -> LRandom
  | LCmp (e1, c, e2) -> LCmp (erase_e e1, c, erase_e e2)
  | LAnd (l1, l2) -> LAnd (erase_l l1, erase_l l2)
  | LOr (l1, l2) -> LOr (erase_l l1, erase_l l2)
  | LNot l -> LNot (erase_l l)
