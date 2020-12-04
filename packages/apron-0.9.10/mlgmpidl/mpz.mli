(* File generated from mpz.idl *)

(* This file is part of the MLGmpIDL interface, released under LGPL license.
   Please read the COPYING file packaged in the distribution  *)

type t

(** GMP multi-precision integers *)


(** The following operations are mapped as much as possible to their C counterpart. In case of imperative functions (like [set], [add], ...) the first parameter of type [t] is an out-parameter and holds the result when the function returns. For instance, [add x y z] adds the values of [y] and [z] and stores the result in [x].

 These functions are as efficient as their C counterpart: they do not imply additional memory allocation, unlike the corresponding functions in the module {!Mpzf}. *)



(** {2 Pretty printing} *)

val print : Format.formatter -> t -> unit

(** {2 Initialization Functions} *)
(** {{:http://gmplib.org/manual/Initializing-Integers.html#Initializing-Integers}C documentation} *)

external init : unit -> t
	= "camlidl_mpz_mpz_init"

external init2 : int -> t
	= "camlidl_mpz_mpz_init2"

external realloc2 : t -> int -> unit
	= "camlidl_mpz_mpz_realloc2"


(** {2 Assignement Functions} *)
(** {{:http://gmplib.org/manual/Assigning-Integers.html#Assigning-Integers}C documentation} *)


(** The first parameter holds the result. *)

external set : t -> t -> unit
	= "camlidl_mpz_mpz_set"

external set_si : t -> int -> unit
	= "camlidl_mpz_mpz_set_si"

external set_d : t -> float -> unit
	= "camlidl_mpz_mpz_set_d"


(** For [set_q: t -> Mpq.t -> unit], see {!Mpq.get_z} *)


external _set_str : t -> string -> int -> unit
	= "camlidl_mpz_mpz__set_str"

val set_str : t -> string -> base:int -> unit
external swap : t -> t -> unit
	= "camlidl_mpz_mpz_swap"


(** {2 Combined Initialization and Assignment Functions} *)
(** {{:http://gmplib.org/manual/Simultaneous-Integer-Init-_0026-Assign.html#Simultaneous-Integer-Init-_0026-Assign}C documentation} *)

external init_set : t -> t
	= "camlidl_mpz_mpz_init_set"

external init_set_si : int -> t
	= "camlidl_mpz_mpz_init_set_si"

external init_set_d : float -> t
	= "camlidl_mpz_mpz_init_set_d"

external _init_set_str : string -> int -> t
	= "camlidl_mpz_mpz__init_set_str"

val init_set_str : string -> base:int -> t

(** {2 Conversion Functions} *)
(** {{:http://gmplib.org/manual/Converting-Integers.html#Converting-Integers}C documentation} *)

external get_si : t -> nativeint
	= "camlidl_mpz_mpz_get_si"

external get_int : t -> int
	= "camlidl_mpz_mpz_get_int"

external get_d : t -> float
	= "camlidl_mpz_mpz_get_d"

external get_d_2exp : t -> float * int
	= "camlidl_mpz_mpz_get_d_2exp"

external _get_str : int -> t -> string
	= "camlidl_mpz_mpz__get_str"

val get_str : base:int -> t -> string


(** {2 User Conversions} *)

(** These functions are additions to or renaming of functions offered by the C library. *)

val to_string : t -> string
val to_float : t -> float
val of_string : string -> t
val of_float : float -> t
val of_int : int -> t

(** {2 Arithmetic Functions} *)
(** {{:http://gmplib.org/manual/Integer-Arithmetic.html#Integer-Arithmetic}C documentation} *)

(** The first parameter holds the result. *)

external add : t -> t -> t -> unit
	= "camlidl_mpz_mpz_add"

external add_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_add_ui"

external sub : t -> t -> t -> unit
	= "camlidl_mpz_mpz_sub"

external sub_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_sub_ui"

external ui_sub : t -> int -> t -> unit
	= "camlidl_mpz_mpz_ui_sub"

external mul : t -> t -> t -> unit
	= "camlidl_mpz_mpz_mul"

external mul_si : t -> t -> int -> unit
	= "camlidl_mpz_mpz_mul_si"

external addmul : t -> t -> t -> unit
	= "camlidl_mpz_mpz_addmul"

external addmul_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_addmul_ui"

external submul : t -> t -> t -> unit
	= "camlidl_mpz_mpz_submul"

external submul_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_submul_ui"

external mul_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_mul_2exp"

external neg : t -> t -> unit
	= "camlidl_mpz_mpz_neg"

external abs : t -> t -> unit
	= "camlidl_mpz_mpz_abs"


(** {2 Division Functions} *)
(** {{:http://gmplib.org/manual/Integer-Division.html#Integer-Division}C documentation} *)

(** [c] stands for ceiling, [f] for floor, and [t] for truncate (rounds toward 0).*)
(** {3 Ceiling division} *)

(** The first parameter holds the quotient. *)
external cdiv_q : t -> t -> t -> unit
	= "camlidl_mpz_mpz_cdiv_q"

(** The first parameter holds the remainder. *)
external cdiv_r : t -> t -> t -> unit
	= "camlidl_mpz_mpz_cdiv_r"

(** The two first parameters hold resp. the quotient and the remainder). *)
external cdiv_qr : t -> t -> t -> t -> unit
	= "camlidl_mpz_mpz_cdiv_qr"

(** The first parameter holds the quotient. *)
external cdiv_q_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_cdiv_q_ui"

(** The first parameter holds the remainder. *)
external cdiv_r_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_cdiv_r_ui"

(** The two first parameters hold resp. the quotient and the remainder). *)
external cdiv_qr_ui : t -> t -> t -> int -> int
	= "camlidl_mpz_mpz_cdiv_qr_ui"

external cdiv_ui : t -> int -> int
	= "camlidl_mpz_mpz_cdiv_ui"

(** The first parameter holds the quotient. *)
external cdiv_q_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_cdiv_q_2exp"

(** The first parameter holds the remainder. *)
external cdiv_r_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_cdiv_r_2exp"

(** {3 Floor division} *)

external fdiv_q : t -> t -> t -> unit
	= "camlidl_mpz_mpz_fdiv_q"

external fdiv_r : t -> t -> t -> unit
	= "camlidl_mpz_mpz_fdiv_r"

external fdiv_qr : t -> t -> t -> t -> unit
	= "camlidl_mpz_mpz_fdiv_qr"

external fdiv_q_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_fdiv_q_ui"

external fdiv_r_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_fdiv_r_ui"

external fdiv_qr_ui : t -> t -> t -> int -> int
	= "camlidl_mpz_mpz_fdiv_qr_ui"

external fdiv_ui : t -> int -> int
	= "camlidl_mpz_mpz_fdiv_ui"

external fdiv_q_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_fdiv_q_2exp"

external fdiv_r_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_fdiv_r_2exp"

(** {3 Truncate division} *)

external tdiv_q : t -> t -> t -> unit
	= "camlidl_mpz_mpz_tdiv_q"

external tdiv_r : t -> t -> t -> unit
	= "camlidl_mpz_mpz_tdiv_r"

external tdiv_qr : t -> t -> t -> t -> unit
	= "camlidl_mpz_mpz_tdiv_qr"

external tdiv_q_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_tdiv_q_ui"

external tdiv_r_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_tdiv_r_ui"

external tdiv_qr_ui : t -> t -> t -> int -> int
	= "camlidl_mpz_mpz_tdiv_qr_ui"

external tdiv_ui : t -> int -> int
	= "camlidl_mpz_mpz_tdiv_ui"

external tdiv_q_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_tdiv_q_2exp"

external tdiv_r_2exp : t -> t -> int -> unit
	= "camlidl_mpz_mpz_tdiv_r_2exp"

(** {3 Other division-related functions} *)

external gmod : t -> t -> t -> unit
	= "camlidl_mpz_mpz_gmod"

external gmod_ui : t -> t -> int -> int
	= "camlidl_mpz_mpz_gmod_ui"

external divexact : t -> t -> t -> unit
	= "camlidl_mpz_mpz_divexact"

external divexact_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_divexact_ui"

external divisible_p : t -> t -> bool
	= "camlidl_mpz_mpz_divisible_p"

external divisible_ui_p : t -> int -> bool
	= "camlidl_mpz_mpz_divisible_ui_p"

external divisible_2exp_p : t -> int -> bool
	= "camlidl_mpz_mpz_divisible_2exp_p"

external congruent_p : t -> t -> t -> bool
	= "camlidl_mpz_mpz_congruent_p"

external congruent_ui_p : t -> int -> int -> bool
	= "camlidl_mpz_mpz_congruent_ui_p"

external congruent_2exp_p : t -> t -> int -> bool
	= "camlidl_mpz_mpz_congruent_2exp_p"


(** {2 Exponentiation Functions} *)
(** {{:http://gmplib.org/manual/Integer-Exponentiation.html#Integer-Exponentiation}C documentation} *)

external _powm : t -> t -> t -> t -> unit
	= "camlidl_mpz_mpz__powm"

external _powm_ui : t -> t -> int -> t -> unit
	= "camlidl_mpz_mpz__powm_ui"

val powm : t -> t -> t -> modulo:t -> unit
val powm_ui : t -> t -> int -> modulo:t -> unit

external pow_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_pow_ui"

external ui_pow_ui : t -> int -> int -> unit
	= "camlidl_mpz_mpz_ui_pow_ui"


(** {2 Root Extraction Functions} *)
(** {{:http://gmplib.org/manual/Integer-Roots.html#Integer-Roots}C documentation} *)

external root : t -> t -> int -> bool
	= "camlidl_mpz_mpz_root"

external sqrt : t -> t -> unit
	= "camlidl_mpz_mpz_sqrt"

external _sqrtrem : t -> t -> t -> unit
	= "camlidl_mpz_mpz__sqrtrem"

val sqrtrem : t -> remainder:t -> t -> unit
external perfect_power_p : t -> bool
	= "camlidl_mpz_mpz_perfect_power_p"

external perfect_square_p : t -> bool
	= "camlidl_mpz_mpz_perfect_square_p"


(** {2 Number Theoretic  Functions} *)
(** {{:http://gmplib.org/manual/Number-Theoretic-Functions.html#Number-Theoretic-Functions}C documentation} *)

external probab_prime_p : t -> int -> int
	= "camlidl_mpz_mpz_probab_prime_p"

external nextprime : t -> t -> unit
	= "camlidl_mpz_mpz_nextprime"

external gcd : t -> t -> t -> unit
	= "camlidl_mpz_mpz_gcd"

external gcd_ui : t option -> t -> int -> int
	= "camlidl_mpz_mpz_gcd_ui"

external _gcdext : t -> t -> t -> t -> t -> unit
	= "camlidl_mpz_mpz__gcdext"

val gcdext : gcd:t -> alpha:t -> beta:t -> t -> t -> unit
external lcm : t -> t -> t -> unit
	= "camlidl_mpz_mpz_lcm"

external lcm_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_lcm_ui"

external invert : t -> t -> t -> bool
	= "camlidl_mpz_mpz_invert"

external jacobi : t -> t -> int
	= "camlidl_mpz_mpz_jacobi"

external legendre : t -> t -> int
	= "camlidl_mpz_mpz_legendre"

external kronecker : t -> t -> int
	= "camlidl_mpz_mpz_kronecker"

external kronecker_si : t -> int -> int
	= "camlidl_mpz_mpz_kronecker_si"

external si_kronecker : int -> t -> int
	= "camlidl_mpz_mpz_si_kronecker"

external remove : t -> t -> t -> int
	= "camlidl_mpz_mpz_remove"

external fac_ui : t -> int -> unit
	= "camlidl_mpz_mpz_fac_ui"

external bin_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_bin_ui"

external bin_uiui : t -> int -> int -> unit
	= "camlidl_mpz_mpz_bin_uiui"

external fib_ui : t -> int -> unit
	= "camlidl_mpz_mpz_fib_ui"

external fib2_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_fib2_ui"

external lucnum_ui : t -> int -> unit
	= "camlidl_mpz_mpz_lucnum_ui"

external lucnum2_ui : t -> t -> int -> unit
	= "camlidl_mpz_mpz_lucnum2_ui"


(** {2 Comparison Functions} *)
(** {{:http://gmplib.org/manual/Integer-Comparisons.html#Integer-Comparisons}C documentation} *)

external cmp : t -> t -> int
	= "camlidl_mpz_mpz_cmp"

external cmp_d : t -> float -> int
	= "camlidl_mpz_mpz_cmp_d"

external cmp_si : t -> int -> int
	= "camlidl_mpz_mpz_cmp_si"

external cmpabs : t -> t -> int
	= "camlidl_mpz_mpz_cmpabs"

external cmpabs_d : t -> float -> int
	= "camlidl_mpz_mpz_cmpabs_d"

external cmpabs_ui : t -> int -> int
	= "camlidl_mpz_mpz_cmpabs_ui"

external sgn : t -> int
	= "camlidl_mpz_mpz_sgn"


(** {2 Logical and Bit Manipulation Functions} *)
(** {{:http://gmplib.org/manual/Integer-Logic-and-Bit-Fiddling.html#Integer-Logic-and-Bit-Fiddling}C documentation} *)

external gand : t -> t -> t -> unit
	= "camlidl_mpz_mpz_gand"

external ior : t -> t -> t -> unit
	= "camlidl_mpz_mpz_ior"

external xor : t -> t -> t -> unit
	= "camlidl_mpz_mpz_xor"

external com : t -> t -> unit
	= "camlidl_mpz_mpz_com"

external popcount : t -> int
	= "camlidl_mpz_mpz_popcount"

external hamdist : t -> t -> int
	= "camlidl_mpz_mpz_hamdist"

external scan0 : t -> int -> int
	= "camlidl_mpz_mpz_scan0"

external scan1 : t -> int -> int
	= "camlidl_mpz_mpz_scan1"

external setbit : t -> int -> unit
	= "camlidl_mpz_mpz_setbit"

external clrbit : t -> int -> unit
	= "camlidl_mpz_mpz_clrbit"

external tstbit : t -> int -> bool
	= "camlidl_mpz_mpz_tstbit"


(** {2 Input and Output Functions: not interfaced} *)


(** {2 Random Number Functions: see {!Gmp_random} module} *)


(** {2 Integer Import and Export Functions} *)
(** {{:ttp://gmplib.org/manual/Integer-Import-and-Export.html#Integer-Import-and-Export}C documentation} *)

external _import : t -> (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> int -> unit
	= "camlidl_mpz_mpz__import"

external _export : t -> int -> int -> (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
	= "camlidl_mpz_mpz__export"

val import : dest:t -> (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> order:int -> endian:int -> unit
val export : t -> order:int -> endian:int -> (int, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t


(** {2 Miscellaneous Functions} *)
(** {{:http://gmplib.org/manual/Miscellaneous-Integer-Functions.html#Miscellaneous-Integer-Functions}C documentation} *)


(* Does it fit in an OCaml integer *)
external fits_int_p : t -> bool
	= "camlidl_mpz_mpz_fits_int_p"

external odd_p : t -> bool
	= "camlidl_mpz_mpz_odd_p"

external even_p : t -> bool
	= "camlidl_mpz_mpz_even_p"

external size : t -> int
	= "camlidl_mpz_mpz_size"

external sizeinbase : t -> int -> int
	= "camlidl_mpz_mpz_sizeinbase"


(* Limited relevance here *)
external fits_ulong_p : t -> bool
	= "camlidl_mpz_mpz_fits_ulong_p"

external fits_slong_p : t -> bool
	= "camlidl_mpz_mpz_fits_slong_p"

external fits_uint_p : t -> bool
	= "camlidl_mpz_mpz_fits_uint_p"

external fits_sint_p : t -> bool
	= "camlidl_mpz_mpz_fits_sint_p"

external fits_ushort_p : t -> bool
	= "camlidl_mpz_mpz_fits_ushort_p"

external fits_sshort_p : t -> bool
	= "camlidl_mpz_mpz_fits_sshort_p"

