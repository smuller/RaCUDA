(** Association tables over ordered types, parametrized polymorphic version *)

(** Same interface as {!Mappe}, but each map stores its comparison
    function.

    If compiled without [-noassert] option, checks for binary
    operations that the two arguments refers to the same
    comparison function (by testing physical equality).

    BE CAUTIOUS: do not use [Pervasives.compare] function
    directly, as it is an external function, so that writing
    [t1.cmp <- Pervasives.compare] induces the creation of a
    closure, and if later [t2.cmp <- Pervasives.compare] is
    executed, [t1.cmp != t2.cmp] because two different closures
    have been created. Instead, first define [let compare = fun x
    y -> Pervasives.compare x y in ...].
*)

type ('a,'b) t = {
  compare : ('a -> 'a -> int);
  map : ('a,'b) Mappe.t
}

val is_empty : ('a,'b) t -> bool
val empty: ('a -> 'a -> int) -> ('a,'b) t
val add: 'a -> 'b -> ('a,'b) t -> ('a,'b) t
val find: 'a -> ('a,'b) t -> 'b
val remove: 'a -> ('a,'b) t -> ('a,'b) t
val mem:  'a -> ('a,'b) t -> bool
val addmap : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val merge : ('b -> 'b -> 'b) -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val mergei : ('a -> 'b -> 'b -> 'b) -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
val common : ('b -> 'c -> 'd) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
val commoni : ('a -> 'b -> 'c -> 'd) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
val combine : ('a -> 'b option -> 'c option -> 'd option) -> ('a,'b) t -> ('a,'c) t -> ('a,'d) t
val interset : ('a,'b) t -> 'a PSette.t -> ('a,'b) t
val diffset : ('a,'b) t -> 'a PSette.t -> ('a,'b) t
val iter: ('a -> 'b -> unit) -> ('a,'b) t -> unit
val map: ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
val mapi: ('a -> 'b -> 'c) -> ('a,'b) t -> ('a,'c) t
val fold: ('a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
val maptoset: ('a,'b) t -> 'a PSette.t
val mapofset: ('a -> 'b) -> 'a PSette.t -> ('a,'b) t
val compare: ('b -> 'c -> int) -> ('a,'b) t -> ('a,'c) t -> int
val comparei: ('a -> 'b -> 'c -> int) -> ('a,'b) t -> ('a,'c) t -> int
val equal: ('b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val equali: ('a -> 'b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val subset: ('b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val subseti: ('a -> 'b -> 'c -> bool) -> ('a,'b) t -> ('a,'c) t -> bool
val filter: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t
val partition: ('a -> 'b -> bool) -> ('a,'b) t -> ('a,'b) t * ('a,'b) t
val cardinal: ('a,'b) t -> int
val bindings : ('a,'b) t -> ('a * 'b) list
val min_key: ('a,'b) t -> 'a
val max_key: ('a,'b) t -> 'a
val choose : ('a,'b) t -> 'a * 'b
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
