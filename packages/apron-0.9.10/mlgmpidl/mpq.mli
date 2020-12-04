(* File generated from mpq.idl *)

(* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type t

(** GMP multiprecision rationals *)


(** The following operations are mapped as much as possible to their C counterpart. In case of imperative functions (like [set], [add], ...) the first parameter of type [t] is an out-parameter and holds the result when the function returns. For instance, [add x y z] adds the values of [y] and [z] and stores the result in [x].

 These functions are as efficient as their C counterpart: they do not imply additional memory allocation, unlike the corresponding functions in the module {!Mpqf}. *)


external canonicalize : t -> unit
	= "camlidl_mpq_mpq_canonicalize"


(** {2 Pretty printing} *)

val print : Format.formatter -> t -> unit

(** {2 Initialization and Assignment Functions} *)
(** {{:http://gmplib.org/manual/Initializing-Rationals.html#Initializing-Rationals}C documentation} *)

external init : unit -> t
	= "camlidl_mpq_mpq_init"

external set : t -> t -> unit
	= "camlidl_mpq_mpq_set"

external set_z : t -> Mpz.t -> unit
	= "camlidl_mpq_mpq_set_z"

external set_si : t -> int -> int -> unit
	= "camlidl_mpq_mpq_set_si"

external _set_str : t -> string -> int -> unit
	= "camlidl_mpq_mpq__set_str"

val set_str : t -> string -> base:int -> unit
external swap : t -> t -> unit
	= "camlidl_mpq_mpq_swap"

(** {2 Additional Initialization and Assignements functions} *)

(** These functions are additions to or renaming of functions offered by the C library. *)

val init_set : t -> t
val init_set_z : Mpz.t -> t
val init_set_si : int -> int -> t
val init_set_str : string -> base:int -> t
val init_set_d : float -> t

(** {2 Conversion Functions} *)
(** {{:http://gmplib.org/manual/Rational-Conversions.html#Rational-Conversions}C documentation} *)

external get_d : t -> float
	= "camlidl_mpq_mpq_get_d"

external set_d : t -> float -> unit
	= "camlidl_mpq_mpq_set_d"


(* Replace Mpz.set_q: Mpz.t -> Mpq.t -> unit *)
external get_z : Mpz.t -> t -> unit
	= "camlidl_mpq_mpq_get_z"


(* For set_f: t -> Mpf.t -> unit, see Mpf.get_q *)


external _get_str : int -> t -> string
	= "camlidl_mpq_mpq__get_str"

val get_str : base:int -> t -> string

(** {2 User Conversions} *)

(** These functionss are additions to or renaming of functions offeered by the C library. *)

val to_string : t -> string
val to_float : t -> float
val of_string : string -> t
val of_float : float -> t
val of_int : int -> t
val of_frac : int -> int -> t
val of_mpz : Mpz.t -> t
val of_mpz2 : Mpz.t -> Mpz.t -> t

(** {2 Arithmetic Functions} *)
(** {{:http://gmplib.org/manual/Rational-Arithmetic.html#Rational-Arithmetic}C documentation} *)

external add : t -> t -> t -> unit
	= "camlidl_mpq_mpq_add"

external sub : t -> t -> t -> unit
	= "camlidl_mpq_mpq_sub"

external mul : t -> t -> t -> unit
	= "camlidl_mpq_mpq_mul"

external mul_2exp : t -> t -> int -> unit
	= "camlidl_mpq_mpq_mul_2exp"

external div : t -> t -> t -> unit
	= "camlidl_mpq_mpq_div"

external div_2exp : t -> t -> int -> unit
	= "camlidl_mpq_mpq_div_2exp"

external neg : t -> t -> unit
	= "camlidl_mpq_mpq_neg"

external abs : t -> t -> unit
	= "camlidl_mpq_mpq_abs"

external inv : t -> t -> unit
	= "camlidl_mpq_mpq_inv"


(** {2 Comparison Functions} *)
(** {{:http://gmplib.org/manual/Comparing-Rationals.html#Comparing-Rationals}C documentation} *)

external cmp : t -> t -> int
	= "camlidl_mpq_mpq_cmp"

external cmp_si : t -> int -> int -> int
	= "camlidl_mpq_mpq_cmp_si"

external sgn : t -> int
	= "camlidl_mpq_mpq_sgn"

external equal : t -> t -> bool
	= "camlidl_mpq_mpq_equal"


(** {2 Applying Integer Functions to Rationals} *)
(** {{:http://gmplib.org/manual/Applying-Integer-Functions.html#Applying-Integer-Functions}C documentation} *)

external get_num : Mpz.t -> t -> unit
	= "camlidl_mpq_mpq_get_num"

external get_den : Mpz.t -> t -> unit
	= "camlidl_mpq_mpq_get_den"

external set_num : t -> Mpz.t -> unit
	= "camlidl_mpq_mpq_set_num"

external set_den : t -> Mpz.t -> unit
	= "camlidl_mpq_mpq_set_den"


(** {2 Input and Output Functions: not interfaced} *)

