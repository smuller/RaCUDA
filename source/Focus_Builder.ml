(* Quentin Carbonneaux - 2016-2017 *)

open Polynom

(* Polynom helper functions. *)

let poly_binom k p =
  if k < 0 then failwith "invalid argument" else
  let rec prod k accu =
    let k = k - 1 in
    if k < 0 then accu else
    let p_min_k = Poly.sub p (Poly.const (float_of_int k)) in
    prod k (Poly.mul p_min_k accu)
  in prod k (Poly.const 1.)

(* Focus functions can be build incrementally by
   composing smaller lemmas defined below.
*)

open Focus_Types

type 'a property =
  | Ge0 of 'a        (* a >= 0 *)
  | Ge of 'a * 'a    (* a >= b *)

let prop_Ge0 = function
  | Ge0 p -> p
  | Ge (p1, p2) -> Poly.sub p1 p2

let prop_Ge = function
  | Ge0 p -> p, Poly.zero ()
  | Ge (p1, p2) -> p1, p2

type t =
  { proves: Poly.t property
  ; checks: Poly.t list
  ; degree: int
  ; ast: ast }

let export i =
  let checks = List.filter
    (fun p ->
      match Poly.is_const p with
      | Some k -> k < 0.
      | None -> true) i.checks
  in
  { Focus_Types.checks = checks
  ; proves = prop_Ge0 i.proves
  ; ast = i.ast
  }

let degree i =
  i.degree

(* Focus function building blocks:

   a >= b                            when a >= b  (check_ge)

   max(0, a) >= 0                                 (max0_ge_0)
   max(0, a) >= a                                 (max0_ge_arg)
   a >= max(0, a)                    when a >= 0  (max0_le_arg)
   max(0, a) >= max(0, b)            when a >= b  (max0_monotonic)
   max(0, b) + a - b >= max(0, a)    when a >= b  (max0_sublinear)

   binom(k, a) >= binom(k, b)        when a >= b  (binom_monotonic)
                                     and b >= 0
   x * a >= x * b                    when x >= 0  (product)
                                     and a >= b

   ----- Focus functions for automation ----

   max(0, x) >= y + max(0, x - y)    when x >= y >= 0 (max0_pre_decrement)
   max(0, x) + y >= max(0, x + y)    when y >= 0      (max0_pre_increment)
*)

let max0_pre_decrement k x y =
  let xsuby = Poly.sub x y in
  let lhs = poly_binom k (poly_max x) in
  let rhs = poly_binom k (Poly.add (poly_max xsuby) y) in
  { proves = Ge (lhs, rhs)
  ; checks = [xsuby; y]
  ; degree = k * max (Poly.degree x) (Poly.degree y)
  ; ast = F_max0_pre_decrement (k, x, y) }

let max0_pre_increment k x y =
  let lhs = poly_binom k (Poly.add (poly_max x) y) in
  let rhs = poly_binom k (poly_max (Poly.add x y)) in
  { proves = Ge (lhs , rhs)
  ; checks = [y]
  ; degree = k * max (Poly.degree x) (Poly.degree y)
  ; ast = F_max0_pre_increment (k, x, y) }

let check_ge a b =
  let checks =
    let sub = Poly.sub a b in
    match Poly.is_const sub with
    | Some k when k >= 0. -> []
    | _ -> [sub]
  in
  { proves = Ge (a, b)
  ; checks = checks
  ; degree = max (Poly.degree a) (Poly.degree b)
  ; ast = F_check_ge (a, b) }

let max0_ge_0 a =
  { proves = Ge0 (poly_max a)
  ; checks = []
  ; degree = Poly.degree a
  ; ast = F_max0_ge_0 a }

let max0_ge_arg a =
  { proves = Ge (poly_max a, a)
  ; checks = []
  ; degree = Poly.degree a
  ; ast = F_max0_ge_arg a }

let max0_le_arg i =
  let a = prop_Ge0 i.proves in
  { proves = Ge (a, poly_max a)
  ; checks = i.checks
  ; degree = i.degree
  ; ast = F_max0_le_arg i.ast }

let max0_monotonic i =
  let a, b = prop_Ge i.proves in
  { proves = Ge (poly_max a, poly_max b)
  ; checks = i.checks
  ; degree = i.degree
  ; ast = F_max0_monotonic i.ast }

let max0_sublinear i =
  let a, b = prop_Ge i.proves in
  { proves = Ge (Poly.add (poly_max b) (Poly.sub a b), poly_max a)
  ; checks = i.checks
  ; degree = i.degree
  ; ast = F_max0_sublinear i.ast }

let binom_monotonic k i1 i2 =
  let a, b = prop_Ge i1.proves in
  let b' = prop_Ge0 i2.proves in
  if Poly.compare b b' <> 0 then begin
    Format.eprintf "(%a) <> (%a)@."
      Poly.print_ascii b Poly.print_ascii b';
    failwith "invalid argument combination ";
  end else
  { proves = Ge (poly_binom k a, poly_binom k b)
  ; checks = i1.checks @ i2.checks
  ; degree = i1.degree * k
  ; ast = F_binom_monotonic (k, i1.ast, i2.ast) }

let product i1 i2 =
  let a, b = prop_Ge i2.proves in
  let x = prop_Ge0 i1.proves in
  { proves = Ge (Poly.mul x a, Poly.mul x b)
  ; checks = i1.checks @ i2.checks
  ; degree = i1.degree + i2.degree
  ; ast = F_product (i1.ast, i2.ast)  }
