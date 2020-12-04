(* Bertrand Jeannet and Peter Schrammel. This file is released under LGPL license. *)

(** Imbricated lists *)

(** The operations of this module have a functional semantics. *)

(** Type of list elements *)
type ('a,'b) el =
| Atome of 'b  (** Terminal case *)
| List of ('a,'b) t (** The element is recursively a list, with an attribute of type ['a]. *)

(** Type of imbricated lists. ['a] is the type of attributes
    associated to lists, and ['b] the type of elements. *)
and ('a,'b) t = 'a * ('a,'b) el list

val cons : ('a,'b) el -> ('a,'b) t -> ('a,'b) t
  (** Adding a new list element at the begining of the list *)
val atome : 'b -> ('a,'b) el
  (** Create a list element from a single element. *)
val list : 'a -> ('a,'b) el list -> ('a,'b) el
  (** Create a list element from a list. *)

val of_list : 'a -> 'b list -> ('a,'b) t
  (** Create a recursive list from a regular list *)
val to_list : ('a,'b) t -> 'b list
  (** Create a regular list from a recursive list.
      Order is preserved but imbrication is lost
      (as in {!flatten}).
      - [to_list [[a;b];c;[d;e]] = [a;b;c;d;e]]
  *)

val hd : ('a,'b) t -> ('a,'b) el
  (** Return the head of the list. *)
val tl : ('a,'b) t -> ('a,'b) t
  (** Return the tail of the list. *)
val length : ('a,'b) t -> int
  (** Return the ength of the list. *)
val depth : ('a,'b) t -> int
  (** Return the (maximal) depth of the list.
    - [depth [] = 0]
    - [depth [a;b;c] = 1]
    - [depth [[a];b] = 2] *)

val append : combine:('a -> 'a -> 'a) -> ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  (** Append two lists *)

val flatten : ?depth:int -> ('a,'b) t -> ('a,'b) t
  (** Flatten the recursive list, only starting from the given

    - [flatten [] = []]
    - [flatten [a;[b;[c];d];e;[f]] = [a;b;c;d;e;f]]
    - [flatten ~depth:2 [a;[b;[c];d];e;[f]] = [a;[b;c;d];e;[f]]]
    - [flatten ~depth:3 [a;[b;[c];d];e;[f]] = [a;[b;[c];d];e;[f]]]
   *)

val rev : ('a,'b) t -> ('a,'b) t
  (** Recursively reverse the recursive list
      - [rev [a;[b;[c];d];e;[f]] = [[f];e;[d;[c];b];a]]
  *)
val mem : 'b -> ('a,'b) t -> bool
  (** Membership test. *)
val exists : ('a -> 'b -> bool) -> ('a,'b) t -> bool
  (** Existence test *)

val map :
  ('a -> 'c) ->
  (bool -> 'a -> 'b -> 'd) ->
  ('a,'b) t -> ('c,'d) t
  (** Ordinary map function *)
val iter : (bool -> 'a -> 'b -> unit) -> ('a,'b) t -> unit
  (** Ordinary iteration function for atoms *)
val fold_left : ('c -> bool -> 'a -> 'b -> 'c) -> 'c -> ('a,'b) t -> 'c
  (** Ordinary fold function for atoms, from left to right. *)
val fold_right : (bool -> 'a -> 'b -> 'c -> 'c) -> ('a,'b) t -> 'c -> 'c
  (** Ordinary fold function for atoms, from right to left. *)

val print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  ?firstexp:(unit, Format.formatter, unit) format ->
  ?lastexp:(unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a,'b) t -> unit
  (** Printing function. *)
