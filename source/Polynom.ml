(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

let fsmall = 1e-7

(* F = x | max(0,P) *)
module type Factor = sig
  type t = Var of Types.id | Max of poly and poly
  val compare: t -> t -> int
  val degree: t -> int
  val var_exists: (Types.id -> bool) -> t -> bool
  val print: Format.formatter -> t -> unit
end

(* M = 1 | x | M1.M2 | max(0,P) *)
module type Monom = sig
  type t and factor
  val is_one: t -> bool
  val compare: t -> t -> int
  val degree: t -> int
  val var_exists: (Types.id -> bool) -> t -> bool
  val fold: (factor -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val pick: t -> (factor * int)
  val one: t
  val of_factor: factor -> int -> t
  val of_var: Types.id -> t
  val pow: int -> t -> t
  val mul_factor: factor -> int -> t -> t
  val mul: t -> t -> t
  val print: ascii: bool -> Format.formatter -> t -> unit
end

(* P = k.M | P1 + P2 *)
module type Poly = sig
  type t and monom
  val zero: unit -> t
  val always_less: t -> t -> bool
  val const: float -> t
  val of_monom: monom -> float -> t
  val of_expr: 'a Types.expr -> t
  val compare: t -> t -> int
  val degree: t -> int
  val var_exists: (Types.id -> bool) -> t -> bool
  val fold: (monom -> float -> 'a -> 'a) -> t -> 'a -> 'a
  val is_const: t -> float option
  val get_coeff: monom -> t -> float
  val scale: float -> t -> t
  val mul_monom: monom -> float -> t -> t
  val add_monom: monom -> float -> t -> t
  val add_scale: float -> t -> t -> t
  val add: t -> t -> t
  val mul: t -> t -> t
  val sub: t -> t -> t
  val pow: int -> t -> t
  val print: Format.formatter -> t -> unit
  val print_ascii: Format.formatter -> t -> unit
end

module MkFactor(Pol: Poly)
: Factor with type poly = Pol.t
= struct

  type poly = Pol.t
  type t =
    | Var of Types.id
    | Max of poly

  let compare a b =
    match a, b with
    | Var v1, Var v2 -> compare v1 v2
    | Max p1, Max p2 -> Pol.compare p1 p2
    | Var _, Max _ -> -1
    | Max _, Var _ -> +1

  let degree = function
    | Var _ -> 1
    | Max p -> Pol.degree p

  let var_exists pred = function
    | Var v -> pred v
    | Max p -> Pol.var_exists pred p

  let print fmt = function
    | Var v -> Format.fprintf fmt "%s" v
    | Max p -> Format.fprintf fmt "max(0, %a)" Pol.print p

end

module MkMonom(Fac: Factor)
: Monom with type factor = Fac.t
= struct

  (* Monomials of a polynomial are maps from factors
     to their power in the monomial.
     e.g., M = x^2 * y is represented as a map:
     (x --> 2, y --> 1)
  *)

  module M = Map.Make(Fac)
  type factor = Fac.t
  type t = int M.t

  let is_one = M.is_empty
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold
  let one = M.empty
  let pick = M.choose

  let degree m =
    fold (fun f e d -> d + e * Fac.degree f) m 0

  let var_exists pred m =
    fold (fun f _ b -> b || Fac.var_exists pred f) m false

  let of_factor f e =
    if e = 0 then one else M.singleton f e

  let of_var v =
    of_factor (Fac.Var v) 1

  let get_pow f m =
    try M.find f m with Not_found -> 0

  let pow e =
    M.map (( * ) e)

  (* m : f --> e' then m : f --> (e + e') *)
  let mul_factor f e m =
    if e = 0 then m else
    M.add f (e + get_pow f m) m

  (* perform m * m' *)
  let mul m m' =
    if is_one m then m' else
    if is_one m' then m else
    fold mul_factor m' m

  let superdigit =
    [| "⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹" |]

  let print ~ascii fmt m =
    let superscript n =
      if n = 1 then "" else
      if ascii then "^" ^ string_of_int n else
      let rec go n =
        if n = 0 then "" else
        go (n/10) ^ superdigit.(n mod 10)
      in go n
    in
    Format.fprintf fmt "@[<h>";
    let is_one = fold begin fun f e first ->
        if e = 0 then first else begin
         Format.fprintf fmt
            (if first then "%a%s" else "@ %a%s")
            Fac.print f (superscript e);
          false
        end
      end m true in
    if is_one then Format.fprintf fmt "1";
    Format.fprintf fmt "@]"

end

module MkPoly(Mon: Monom)
: Poly with type monom = Mon.t
= struct

  (* Polynomials are represented as maps from monomials
     to their coefficient.
     e.g., P = 3x^2*y + k is represented as a map:
     (m_0 --> k, m_1 --> 3), where m_0: empty, m_1: (x --> 2, y --> 1)
  *)

  module M = Map.Make(Mon)
  type monom = Mon.t
  type t = float M.t

  let zero = M.empty
  let compare = (M.compare compare: t -> t -> int)
  let fold = M.fold

  let always_less a b = true

  (* p = k.m *)
  let of_monom m k = M.singleton m k

  let degree p =
    fold (fun m _ d -> max d (Mon.degree m)) p 0

  let var_exists pred p =
    fold (fun m _ b -> b || Mon.var_exists pred m) p false

  let const k =
    if abs_float k <= fsmall
      then zero
      else M.singleton Mon.one k

  (* get coefficient of monomial m in polynomial pol *)
  let get_coeff m pol =
    try M.find m pol with Not_found -> 0.

  let is_const pol =
    if M.is_empty pol then Some 0. else
    if M.cardinal pol <> 1 then None else
    try Some (M.find Mon.one pol) with Not_found -> None

  let scale k pol =
    fold begin fun m k' res ->
      let k' = k' *. k in
      if abs_float k' <= fsmall
        then res
        else M.add m k' res
    end pol zero

  (* add k.m to pol *)
  let add_monom m k pol =
    let c = get_coeff m pol +. k in
    if abs_float c <= fsmall
      then M.remove m pol
      else M.add m c pol

  (* perform p = scale * p1 + p2 *)
  let add_scale scale =
    let f = function Some c -> c | None -> 0. in
    M.merge (fun _ ac bc ->
      let c = scale *. f ac +. f bc in
      if abs_float c <= fsmall then None else Some c
    )

  (* add p1 p2 performs p1 + p2 *)
  let add = add_scale 1.

  let sub a b = add_scale (-1.) b a

  (* 
    pol : m1 --> c1, m2 --> c2 where
    m1 : f --> e1, m2 : f' --> e2 then 
    pol : m'1 --> c1, m2 --> c2 where
    m'1 : f --> (e + e1)
  *)
  let mul_factor f e pol =
    fold begin fun m coeff res ->
      M.add (Mon.mul_factor f e m) coeff res
    end pol zero

  (* perform p = coeff * m * pol *)
  let mul_monom m coeff pol =
    if abs_float coeff <= fsmall then zero else
    fold begin fun m' coeff' res ->
      M.add (Mon.mul m m') (coeff *. coeff') res
    end pol zero

  (* perform p = p1 * p2 *)
  let mul p1 p2 =
    fold begin fun m coeff res ->
      add (mul_monom m coeff p2) res
    end p1 zero

  (* p = e *)
  let rec of_expr e =
    match Types.desc e with
    | Types.ERandom -> failwith "expression contains random"
    | Types.EVar v -> of_monom (Mon.of_var v) 1.
    | Types.ENum n -> of_monom Mon.one (float_of_int n)
    | Types.EAdd (e1, e2) -> add (of_expr e1) (of_expr e2)
    | Types.ESub (e1, e2) -> sub (of_expr e1) (of_expr e2)
    | Types.EMul (e1, e2) -> mul (of_expr e1) (of_expr e2)
    | Types.EBer (pa, pb) -> Utils._TODO "Bernoulli sampling distribution"
    | Types.EBin (n, pa, pb) -> Utils._TODO "Binomial sampling distribution"
    | Types.EGeo (pa, pb) -> Utils._TODO "Geometric sampling distribution"
    | Types.ENbin (r, pa, pb) -> Utils._TODO "Negative binomial sampling distribution"
    | Types.EPois (la, lb) -> Utils._TODO "Poisson sampling distribution"
    | Types.EHyper (n, r, m) -> Utils._TODO "Hyper-geometric sampling distribution"
    | Types.EUnif (lb, ub) -> Utils._TODO "Uniform sampling distribution"
    | Types.EDist (id, lb, ub, ea, eb) -> Utils._TODO "general sampling distribution"

  let rec pow n pol =
    if n < 0 then failwith "invalid argument" else
    if n = 0 then const 1. else
    mul pol (pow (n-1) pol)

  let print ascii fmt pol =
    Format.fprintf fmt "@[<hov>";
    let is_zero = fold begin fun monom k first ->
        let pref, flt =
          if k < 0.
            then "-", (-. k)
            else (if first then "" else "+"), k
        in
        if Mon.is_one monom then
          Format.fprintf fmt
            (if first then "%s%g" else "@ %s %g")
            pref flt
        else if abs_float (flt -. 1.) <= fsmall then
          Format.fprintf fmt
            (if first then "%s%a" else "@ %s %a")
            pref (Mon.print ~ascii) monom
        else
          Format.fprintf fmt
            (if first then "%s@[<h>%g %a@]" else "@ %s @[<h>%g %a@]")
            pref flt (Mon.print ~ascii) monom;
        false
      end pol true in
    if is_zero then Format.fprintf fmt "0";
    Format.fprintf fmt "@]"

  let print_ascii = print true
  let print = print false

  let zero () = zero

end

module
  rec Poly
  : Poly with type monom = Monom.t
  = MkPoly(Monom)

  and Monom
  : Monom with type factor = Factor.t
  = MkMonom(Factor)

  and Factor
  : Factor with type poly = Poly.t
  = MkFactor(Poly)


let poly_max pol =
  match Poly.is_const pol with
  | Some k -> `Monom (Monom.one, max 0. k)
  | None -> `Factor (Factor.Max pol, 1)

let rec mul_fmp a b =
  let mkmonom = function `Factor (fa, ea) ->
    `Monom (Monom.of_factor fa ea, 1.)
  in match a, b with
  | `Poly pa, `Poly pb -> `Poly (Poly.mul pa pb)
  | `Poly pa, `Monom (mb, kb) -> `Poly (Poly.mul_monom mb kb pa)
  | `Monom (ma, ka), `Poly pb -> `Poly (Poly.mul_monom ma ka pb)
  | `Poly _, (`Factor _ as b) -> mul_fmp a (mkmonom b)
  | (`Factor _ as a), `Poly _ -> mul_fmp (mkmonom a) b
  | `Monom (ma, ka), `Monom (mb, kb) -> `Monom (Monom.mul ma mb, ka *. kb)
  | `Monom (ma, ka), `Factor (fb, eb) -> `Monom (Monom.mul_factor fb eb ma, ka)
  | `Factor (fa, ea), `Monom (mb, kb) -> `Monom (Monom.mul_factor fa ea mb, kb)
  | (`Factor _ as a), (`Factor _ as b) -> mul_fmp (mkmonom a) (mkmonom b)

let rec factor_subst id p = function
  | Factor.Var v when v = id -> `Poly p
  | Factor.Max p' -> poly_max (poly_subst id p p')
  | f -> `Factor (f, 1)

and monom_subst id p m =
  Monom.fold begin fun f e res ->
    let fe =
      match factor_subst id p f with
      | `Poly p -> `Poly (Poly.pow e p)
      | `Monom (m, k) -> `Monom (Monom.pow e m, k ** (float_of_int e))
      | `Factor (f, e') -> `Factor (f, e * e')
    in
    mul_fmp fe res
  end m (`Monom (Monom.one, 1.))

and poly_subst id p p' =
  Poly.fold begin fun m k res ->
    match monom_subst id p m with
    | `Poly p -> Poly.add_scale k p res
    | `Monom (m, k') -> Poly.add_monom m (k' *. k) res
    | `Factor (f, e) -> Poly.add_monom (Monom.of_factor f e) k res
  end p' (Poly.zero ())

let normalize x =
  match x with
  | `Poly p -> p
  | `Monom (m, k) -> Poly.of_monom m k
  | `Factor (f, e) -> Poly.of_monom (Monom.of_factor f e) 1.

let monom_subst id p m = normalize (monom_subst id p m)
let poly_max p = normalize (poly_max p)
