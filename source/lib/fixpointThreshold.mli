(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system: inference of thresholds *)

(*  ********************************************************************** *)
(** {2 Public datatypes} *)
(*  ********************************************************************** *)

(** Manager for thresholds *)
type ('vertex,'hedge,'threshold) parameter = {
  mutable compare : 'threshold -> 'threshold -> int;
    (** Comparison of thresholds (for creating sets of type
	['threshold PSette.t]) *)
  mutable print : Format.formatter -> 'threshold -> unit;
    (** Printing thresholds *)
  mutable init : 'vertex -> 'threshold PSette.t;
    (** Initial thresholds *)
  mutable apply : 'hedge -> 'threshold PSette.t array -> 'threshold PSette.t;
    (** Propagating thresholds (like the [apply] field in type
	{FixpointType.manager}). *)
  mutable iteration_nb : int;
    (**  Number of iterations (advise: 2) *)
}

val inference :
  ('vertex,'hedge,'abstract,'arc) FixpointType.manager ->
  ('vertex,'hedge,'threshold) parameter ->
  ('vertex,'hedge,'a,'b,'c) PSHGraph.t ->
  ('vertex,'hedge) FixpointType.strategy ->
  ('vertex,'threshold PSette.t) PHashhe.t
    (** Inference functions, taking a standard manager, a
	threshold manager, an equation graph, and an iteration
	strategy. *)
