(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(** Association tables over ordered types (extension of standard library module and polymorphic variant) *)

(** This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

(** Modification par B. Jeannet pour avoir des mappes génériques *)

type ('a,'b) map =
    Empty
  | Node of ('a,'b) map * 'a * 'b * ('a,'b) map * int

type ('a,'b) t = ('a,'b) map
      (** The type of maps from type ['a] to type ['b]. *)

val is_empty : ('a,'b) t -> bool
	  (** Is the map empty ? *)
val empty: ('a,'b) t
	  (** The empty map. *)
val add: 'a -> 'b -> ('a,'b) t -> ('a,'b) t
	(** [add x y m] returns a map containing the same bindings as
	   [m], plus a binding of [x] to [y]. If [x] was already bound
	   in [m], its previous binding disappears. *)
val find: 'a -> ('a,'b) t -> 'b
	(** [find x m] returns the current binding of [x] in [m], or raises
	    [Not_found] if no such binding exists. *)
val remove: 'a -> ('a,'b) t -> ('a,'b) t
	(** [remove x m] returns a map containing the same bindings as [m],
	    except for [x] which is unbound in the returned map. *)
val mem:  'a -> ('a,'b) t -> bool
	(** [mem x m] returns [true] if [m] contains a binding for [m],
	    and [false] otherwise. *)
val addmap : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
	(** [addmap m1 m2] merges the two maps [m1] and [m2]. If a key of [m2]
	    was already bound in [m1], the previous binding in [a]
	    disappears. *)
val merge : ('b -> 'b -> 'b) -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
	(** [merge mergedata a b] is similar to [addmap a b], but if a key [k]
	    is bound to [d1] in [m1] and to [d2] in [m2], the key [k] is bound
	    to [mergedata d1 d2] in the result *)
val mergei : ('a -> 'b -> 'b -> 'b) -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
	(** Same as [merge], but the function receives as arguments the key
	    (of the first map) *)
val common : ('b -> 'c -> 'd) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
	(** [common commondata a b] returns a map the keys of which must be
	    keys in both [a] and in [b], and those keys are bound to [interdata
	    d1 d2]. *)
val commoni : ('a -> 'b -> 'c -> 'd) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
	(** Same as [common], but the function receives as arguments the key
	    (of the first map) *)
val combine : ('a -> 'b option -> 'c option -> 'd option) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
val interset : ('a,'b) t -> 'a Sette.t -> ('a,'b) t
	(** [interset map set] selects the bindings in [a] whose key belongs to
	    [set]. *)
val diffset : ('a,'b) t -> 'a Sette.t -> ('a,'b) t
	(** [interset map set] removes the bindings of [a] whose key belongs to
	    [set]. *)
val iter: ('a -> 'b -> unit) -> ('a,'b) t -> unit
	(** [iter f m] applies [f] to all bindings in map [m].
	    [f] receives the key as first argument, and the associated value as
	    second argument. The order in which the bindings are passed to [f]
	    is unspecified. Only current bindings are presented to [f]:
	    bindings hidden by more recent bindings are not passed to [f]. *)
val map: ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
	(** [map f m] returns a map with same domain as [m], where the
	    associated value [a] of all bindings of [m] has been replaced by
	    the result of the application of [f] to [a].  The order in which
	    the associated values are passed to [f] is unspecified. *)
val mapi: ('a -> 'b -> 'c) -> ('a,'b) t -> ('a,'c) t
	(** Same as [map], but the function receives as arguments both the key
	    and the associated value for each binding of the map. *)
val fold: ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
	(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)], where [k1
	    ... kN] are the keys of all bindings in [m], and [d1 ... dN] are
	    the associated data.  The order in which the bindings are presented
	    to [f] is unspecified. *)
val maptoset: ('a,'b) t -> 'a Sette.t
	(** [maptoset m] returns the set of the keys in the association table *)
val mapofset: ('a -> 'b) -> 'a Sette.t -> ('a,'b) t
	(** [mapofset f s] returns the map associating [f key] to
	   [key], for each element [key] of the set [s] *)
val compare: ('b -> 'c -> int) -> ('a,'b) t -> ('a,'c) t -> int
val comparei: ('a -> 'b -> 'c -> int) -> ('a,'b) t -> ('a,'c) t -> int
	(** Comparison function between maps,
	    total if the comparison function for data is total *)
val equal: ('b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val equali: ('a -> 'b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
	(** equality between maps *)
val subset: ('b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val subseti: ('a -> 'b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
	(** subset between maps *)
val filter: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t
	(** [filter p m] returns the map of all bindings in [m] that satisfy
	  predicate [p]. *)
val partition: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t * ('a,'b) t
	(** [partition p m] returns a pair of maps [(m1, m2)], where [m1] is
	  the map of all the bindings of [m] that satisfy the predicate [p],
	  and [m2] is the map of all the bindings of [m] that do not satisfy
	  [p]. *)
val cardinal: ('a,'b) t -> int
	(** Number of keys of a map *)
val bindings : ('a,'b) t -> ('a * 'b) list
    (** Return the list of all bindings of the given map. The
	returned list is sorted in increasing order with respect to
	the ordering [Pervasives.compare] on keys. *)
val min_key: ('a,'b) t -> 'a
    (** Return the smallest key of the given map or
	raise [Not_found] if the set is empty. *)
val max_key: ('a,'b) t -> 'a
    (** Same as [min_elt], but returns the largest key of the given
       map. *)
val choose : ('a,'b) t -> 'a * 'b
	(** Returns a binding, or raise [Not_found] if empty *)
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
      (** Print a map. *)

(** Output signature of the functor {!Mappe.Make}. *)
module type S = sig
  type key
  type 'a t
  module Setkey : (Sette.S with type elt=key)

  val repr : 'a t -> (key,'a) map
  val obj : (key,'a) map -> 'a t

  val is_empty : 'a t -> bool
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val addmap : 'a t -> 'a t -> 'a t
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val mergei : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val common : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val commoni : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val combine : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val interset : 'a t -> Setkey.t -> 'a t
  val diffset : 'a t -> Setkey.t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val maptoset : 'a t -> Setkey.t
  val mapofset: (key -> 'a) -> Setkey.t -> 'a t
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val comparei : (key -> 'a -> 'b -> int) -> 'a t -> 'b t -> int
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val equali : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val subset : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val subseti : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_key: 'a t -> key
  val max_key: 'a t -> key
  val choose : 'a t -> key * 'a
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

(** Functor building an implementation of the map structure
   given a totally ordered type. *)
module Make(Setkey : Sette.S) : (S with type key=Setkey.elt
				    and module Setkey=Setkey)

module Compare : sig
  val add : ('a -> 'a -> int) -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : ('a -> 'a -> int) -> 'a -> ('a, 'b) t -> 'b
  val remove : ('a -> 'a -> int) -> 'a -> ('a, 'b) t -> ('a, 'b) t
  val mem : ('a -> 'a -> int) -> 'a -> ('a, 'b) t -> bool
  val addmap : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val merge :
    ('a -> 'a -> int) ->
    ('b -> 'b -> 'b) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val mergei :
    ('a -> 'a -> int) ->
    ('a -> 'b -> 'b -> 'b) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val common :
    ('a -> 'a -> int) ->
    ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  val commoni :
    ('a -> 'a -> int) ->
    ('a -> 'b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  val combine :
    ('a -> 'a -> int) ->
    ('a -> 'b option -> 'c option -> 'd option) ->
    ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  val interset : ('a -> 'a -> int) -> ('a, 'b) t -> 'a Sette.t -> ('a, 'b) t
  val diffset : ('a -> 'a -> int) -> ('a, 'b) t -> 'a Sette.t -> ('a, 'b) t
  val compare :
    ('a -> 'a -> int) ->
    ('b -> 'c -> int) -> ('a, 'b) t -> ('a, 'c) t -> int
  val comparei :
    ('a -> 'a -> int) ->
    ('a -> 'b -> 'c -> int) -> ('a, 'b) t -> ('a, 'c) t -> int
  val equal :
    ('a -> 'a -> int) ->
    ('b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
  val equali :
    ('a -> 'a -> int) ->
    ('a -> 'b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
  val subset :
    ('a -> 'a -> int) ->
    ('b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
  val subseti :
    ('a -> 'a -> int) ->
    ('a -> 'b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
  val filter :
    ('a -> 'a -> int) ->
    ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
  val partition :
    ('a -> 'a -> int) ->
    ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
end
