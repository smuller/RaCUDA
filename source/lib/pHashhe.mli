(** Hash tables, parametrized polymorphic version *)

(** Same interface as {!Hashhe}, but each hash table stores its comparison
    functions. *)

type 'a compare = 'a Hashhe.compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

type ('a, 'b) t = {
  compare : 'a Hashhe.compare;
  mutable hashtbl : ('a, 'b) Hashhe.t
}

val stdcompare : 'a compare

val create : ('a -> int) -> ('a -> 'a -> bool) -> int -> ('a, 'b) t
val create_compare : 'a Hashhe.compare -> int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val remove : ('a, 'b) t -> 'a -> unit
val find : ('a, 'b) t -> 'a -> 'b
val find_all : ('a, 'b) t -> 'a -> 'b list
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val mem : ('a, 'b) t -> 'a -> bool
val copy : ('a, 'b) t -> ('a, 'b) t
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val map : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val length : ('a, 'b) t -> int
val print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  ?firstbind:(unit, Format.formatter, unit) format ->
  ?sepbind:(unit, Format.formatter, unit) format ->
  ?lastbind:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a, 'b) t -> unit

