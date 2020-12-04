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

type expr =
  | ERandom
  | EVar of id
  | ENum of int
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EBer of int * int
  | EBin of int * int * int
  | EGeo of int * int
  | ENbin of int * int * int
  | EPois of int * int
  | EHyper of int * int * int
  | EUnif of int * int
  | EDist of id * int * int * int * int

type prob_expr =
  | EProb of int * int

type free_expr =
  | FBase of expr
  | FApply of id * free_expr list * position

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

type logic =
  | LTrue
  | LFalse
  | LRandom
  | LCmp of expr * cmp * expr
  | LAnd of logic * logic
  | LOr of logic * logic
  | LNot of logic
