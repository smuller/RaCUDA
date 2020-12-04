(* File generated from mpf.idl *)

(* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type t

(** GMP multiprecision floating-point numbers *)


(** The following operations are mapped as much as possible to their C counterpart. In case of imperative functions (like [set], [add], ...) the first parameter of type [t] is an out-parameter and holds the result when the function returns. For instance, [add x y z] adds the values of [y] and [z] and stores the result in [x].

 These functions are as efficient as their C counterpart: they do not imply additional memory allocation. *)



(** {2 Initialization Functions} *)
(** {{:http://gmplib.org/manual/Initializing-Floats.html#Initializing-Floats}C documentation} *)

external set_default_prec : int -> unit
	= "camlidl_mpf_mpf_set_default_prec"

external get_default_prec : unit -> int
	= "camlidl_mpf_mpf_get_default_prec"

external init : unit -> t
	= "camlidl_mpf_mpf_init"

external init2 : int -> t
	= "camlidl_mpf_mpf_init2"

external get_prec : t -> int
	= "camlidl_mpf_mpf_get_prec"

external set_prec : t -> int -> unit
	= "camlidl_mpf_mpf_set_prec"

external set_prec_raw : t -> int -> unit
	= "camlidl_mpf_mpf_set_prec_raw"


(** {2 Assignement Functions} *)
(** {{:http://gmplib.org/manual/Assigning-Floats.html#Assigning-Floats}C documentation} *)

external set : t -> t -> unit
	= "camlidl_mpf_mpf_set"

external set_si : t -> int -> unit
	= "camlidl_mpf_mpf_set_si"

external set_d : t -> float -> unit
	= "camlidl_mpf_mpf_set_d"

external set_z : t -> Mpz.t -> unit
	= "camlidl_mpf_mpf_set_z"

external set_q : t -> Mpq.t -> unit
	= "camlidl_mpf_mpf_set_q"

external _set_str : t -> string -> int -> unit
	= "camlidl_mpf_mpf__set_str"

let set_str a b ~base = _set_str a b base
external swap : t -> t -> unit
	= "camlidl_mpf_mpf_swap"


(** {2 Combined Initialization and Assignement Functions} *)
(** {{:http://gmplib.org/manual/Simultaneous-Float-Init-_0026-Assign.html#Simultaneous-Float-Init-_0026-Assign}C documentation} *)

external init_set : t -> t
	= "camlidl_mpf_mpf_init_set"

external init_set_si : int -> t
	= "camlidl_mpf_mpf_init_set_si"

external init_set_d : float -> t
	= "camlidl_mpf_mpf_init_set_d"

external _init_set_str : string -> int -> t
	= "camlidl_mpf_mpf__init_set_str"

let init_set_str a ~base = _init_set_str a base

(** {2 Conversion Functions} *)
(** {{:http://gmplib.org/manual/Converting-Floats.html#Converting-Floats}C documentation} *)

external get_d : t -> float
	= "camlidl_mpf_mpf_get_d"

external get_d_2exp : t -> float * int
	= "camlidl_mpf_mpf_get_d_2exp"

external get_si : t -> nativeint
	= "camlidl_mpf_mpf_get_si"

external get_int : t -> int
	= "camlidl_mpf_mpf_get_int"

external get_z : Mpz.t -> t -> unit
	= "camlidl_mpf_mpf_get_z"

external get_q : Mpq.t -> t -> unit
	= "camlidl_mpf_mpf_get_q"

external _get_str : int -> int -> t -> string * int
	= "camlidl_mpf_mpf__get_str"

let get_str ~base ~digits a = _get_str base digits a

(** {2 Arithmetic Functions} *)
(** {{:http://gmplib.org/manual/Float-Arithmetic.html#Float-Arithmetic}C documentation} *)

external add : t -> t -> t -> unit
	= "camlidl_mpf_mpf_add"

external add_ui : t -> t -> int -> unit
	= "camlidl_mpf_mpf_add_ui"

external sub : t -> t -> t -> unit
	= "camlidl_mpf_mpf_sub"

external ui_sub : t -> int -> t -> unit
	= "camlidl_mpf_mpf_ui_sub"

external sub_ui : t -> t -> int -> unit
	= "camlidl_mpf_mpf_sub_ui"

external mul : t -> t -> t -> unit
	= "camlidl_mpf_mpf_mul"

external mul_ui : t -> t -> int -> unit
	= "camlidl_mpf_mpf_mul_ui"

external mul_2exp : t -> t -> int -> unit
	= "camlidl_mpf_mpf_mul_2exp"

external div : t -> t -> t -> unit
	= "camlidl_mpf_mpf_div"

external ui_div : t -> int -> t -> unit
	= "camlidl_mpf_mpf_ui_div"

external div_ui : t -> t -> int -> unit
	= "camlidl_mpf_mpf_div_ui"

external div_2exp : t -> t -> int -> unit
	= "camlidl_mpf_mpf_div_2exp"

external sqrt : t -> t -> unit
	= "camlidl_mpf_mpf_sqrt"

external pow_ui : t -> t -> int -> unit
	= "camlidl_mpf_mpf_pow_ui"

external neg : t -> t -> unit
	= "camlidl_mpf_mpf_neg"

external abs : t -> t -> unit
	= "camlidl_mpf_mpf_abs"


(** {2 Comparison Functions} *)
(** {{:http://gmplib.org/manual/Float-Comparison.html#Float-Comparison}C documentation} *)

external cmp : t -> t -> int
	= "camlidl_mpf_mpf_cmp"

external cmp_d : t -> float -> int
	= "camlidl_mpf_mpf_cmp_d"

external cmp_si : t -> int -> int
	= "camlidl_mpf_mpf_cmp_si"

external sgn : t -> int
	= "camlidl_mpf_mpf_sgn"

external _equal : t -> t -> int -> bool
	= "camlidl_mpf_mpf__equal"

let equal a b ~bits = _equal a b bits
external reldiff : t -> t -> t -> unit
	= "camlidl_mpf_mpf_reldiff"


(** {2 Input and Output Functions: not interfaced} *)


(** {2 Random Number Functions: see {!Gmp_random} module} *)


(** {2 Miscellaneous Float Functions} *)
(** {{:http://gmplib.org/manual/Miscellaneous-Float-Functions.html#Miscellaneous-Float-Functions}C documentation} *)

external ceil : t -> t -> unit
	= "camlidl_mpf_mpf_ceil"

external floor : t -> t -> unit
	= "camlidl_mpf_mpf_floor"

external trunc : t -> t -> unit
	= "camlidl_mpf_mpf_trunc"

external integer_p : t -> bool
	= "camlidl_mpf_mpf_integer_p"

external fits_int_p : t -> bool
	= "camlidl_mpf_mpf_fits_int_p"

external fits_ulong_p : t -> bool
	= "camlidl_mpf_mpf_fits_ulong_p"

external fits_slong_p : t -> bool
	= "camlidl_mpf_mpf_fits_slong_p"

external fits_uint_p : t -> bool
	= "camlidl_mpf_mpf_fits_uint_p"

external fits_sint_p : t -> bool
	= "camlidl_mpf_mpf_fits_sint_p"

external fits_ushort_p : t -> bool
	= "camlidl_mpf_mpf_fits_ushort_p"

external fits_sshort_p : t -> bool
	= "camlidl_mpf_mpf_fits_sshort_p"


(** {2 Additional functions} *)

let to_string x = 
let (s,e) = get_str ~base:10 ~digits:0 x in 
if s="" 
then "0.0" 
else Format.sprintf "0.%se%i" s e 

let to_float = get_d
let of_string str = init_set_str str 10
let of_float = init_set_d
let of_int a = init_set_si a
let of_mpz x = let res = init() in set_z res x; res
let of_mpq x = let res = init() in set_q res x; res
let is_integer = integer_p

(** {2 Pretty printing} *)

let print fmt x = 
let (s,e) = get_str ~base:10 ~digits:0 x in 
if s="" 
then Format.pp_print_string fmt "0.0" 
else Format.fprintf fmt "0.%se%i" s e 

