(* Quentin Carbonneaux - 2017 *)
module Poly = Polynom.Poly

type ast =
  | F_one
  | F_check_ge of Poly.t * Poly.t
  | F_max0_pre_decrement of int * Poly.t * Poly.t
  | F_max0_pre_increment of int * Poly.t * Poly.t
  | F_max0_ge_0 of Poly.t
  | F_max0_ge_arg of Poly.t
  | F_max0_le_arg of ast
  | F_max0_monotonic of ast
  | F_max0_sublinear of ast
  | F_binom_monotonic of int * ast * ast
  | F_product of ast * ast

type focus =
  { checks: Poly.t list
  ; proves: Poly.t
  ; ast: ast
  }
