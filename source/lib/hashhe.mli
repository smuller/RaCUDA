(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Hash tables and hash functions (extension of standard library module) *)

(** Hash tables are hashed association tables, with in-place modification.

    Modified by B. Jeannet: functions [map], [copy] and [print].
*)

type ('a, 'b) hashtbl

type 'a compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

(** {6 Generic interface} *)

type ('a, 'b) t = ('a, 'b) hashtbl
(** The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a, 'b) t
(** [create n] creates a new, empty hash table, with
   initial size [n].  For best results, [n] should be on the
   order of the expected number of elements that will be in
   the table.  The table grows as needed, so [n] is just an
   initial guess. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)

val reset : ('a, 'b) t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val find : ('a, 'b) t -> 'a -> 'b
(** [find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val mem : ('a, 'b) t -> 'a -> bool
(** [mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
(** [remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!remove}[ tbl x]
   followed by {!add}[ tbl x y]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val map : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** [map f tbl] applies [f] to all bindings in table [tbl] and creates
  a new hashtable associating the results of [f] to the same key type.  [f]
  receives the key as first argument, and the associated value as second
  argument. Each binding is presented exactly once to [f].  The order in which
  the bindings are passed to [f] is unspecified.  However, if the table
  contains several bindings for the same key, they are passed to [f] in reverse
  order of introduction, that is, the most recent binding is passed first. *)

val length : ('a, 'b) t -> int
(** [length tbl] returns the number of bindings in [tbl].
   Multiple bindings are counted multiply, so [length]
   gives the number of times [iter] calls it first argument. *)


type statistics = Hashtbl.statistics = {
  num_bindings: int;
    (** Number of bindings present in the table.
	Same value as returned by {!Hashtbl.length}. *)
  num_buckets: int;
    (** Number of buckets in the table. *)
  max_bucket_length: int;
    (** Maximal number of bindings per bucket. *)
  bucket_histogram: int array
    (** Histogram of bucket sizes.  This array [histo] has
	length [max_bucket_length + 1].  The value of
	[histo.(i)] is the number of buckets whose size is [i]. *)
}

val stats : ('a, 'b) t -> statistics
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
   number of buckets, size of the biggest bucket, distribution of
   buckets by size.
   @since 4.00.0 *)

val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    Format.formatter -> ('a,'b) t -> unit

(** {6 Functorial interface} *)

module type HashedType =
  sig
    type t
      (** The type of the hashtable keys. *)
    val equal : t -> t -> bool
      (** The equality predicate used to compare keys. *)
    val hash : t -> int
      (** A hashing function on keys. It must be such that if two keys are
	  equal according to [equal], then they have identical hash values
	  as computed by [hash].
	  Examples: suitable ([equal], [hash]) pairs for arbitrary key
	  types include
	  ([(=)], {!hash}) for comparing objects by structure,
	  ([(fun x y -> compare x y = 0)], {!hash})
	  for comparing objects by structure and handling {!Pervasives.nan}
	  correctly, and
	  ([(==)], {!hash}) for comparing objects by addresses
	  (e.g. for or cyclic keys). *)
   end
(** The input signature of the functor {!Make}. *)

module type S =
  sig
    type key
    type 'a t = (key,'a) hashtbl
    module Hash : (HashedType with type t=key)

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val length : 'a t -> int
    val stats : 'a t -> statistics
    val print :
	 ?first:(unit, Format.formatter, unit) format ->
	 ?sep:(unit, Format.formatter, unit) format ->
	 ?last:(unit, Format.formatter, unit) format ->
	 ?firstbind:(unit, Format.formatter, unit) format ->
	 ?sepbind:(unit, Format.formatter, unit) format ->
	 ?lastbind:(unit, Format.formatter, unit) format ->
	 (Format.formatter -> key -> unit) ->
	 (Format.formatter -> 'a -> unit) ->
	 Format.formatter -> 'a t -> unit
  end
(** The output signature of the functor {!Make}. *)

module Make (H : HashedType) : S with type key = H.t
(** Functor building an implementation of the hashtable structure.
    The functor [Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing. *)

(** {6 The polymorphic hash primitive} *)


val hash : 'a -> int
(** [hash x] associates a positive integer to any value of
   any type. It is guaranteed that
   if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
   Moreover, [hash] always terminates, even on cyclic
   structures. *)

val hash_param : int -> int -> 'a -> int
(** [hash_param n m x] computes a hash value for [x], with the
   same properties as for [hash]. The two extra parameters [n] and
   [m] give more precise control over hashing. Hashing performs a
   depth-first, right-to-left traversal of the structure [x], stopping
   after [n] meaningful nodes were encountered, or [m] nodes,
   meaningful or not, were encountered. Meaningful nodes are: integers;
   floating-point numbers; strings; characters; booleans; and constant
   constructors. Larger values of [m] and [n] means that more
   nodes are taken into account to compute the final hash
   value, and therefore collisions are less likely to happen.
   However, hashing takes longer. The parameters [m] and [n]
   govern the tradeoff between accuracy and speed. *)

val stdcompare : 'a compare

module Compare : sig
  val add : 'a compare -> ('a, 'b) hashtbl -> 'a -> 'b -> unit
  val remove : 'a compare -> ('a, 'b) hashtbl -> 'a -> unit
  val find : 'a compare -> ('a, 'b) hashtbl -> 'a -> 'b
  val find_all : 'a compare -> ('a, 'b) hashtbl -> 'a -> 'b list
  val replace : 'a compare -> ('a, 'b) hashtbl -> 'a -> 'b -> unit
  val mem : 'a compare -> ('a, 'b) hashtbl -> 'a -> bool
end
