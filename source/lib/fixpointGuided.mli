(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Guided fixpoint analysis of an equation system *)

(** Technique of [Gopand and Reps, SAS'07]  *)

open FixpointType

val analysis :
    ('vertex, 'hedge, 'attr, 'arc) manager ->
    ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
    (('hedge -> bool) -> ('vertex,'hedge) strategy) ->
    ('vertex, 'hedge, 'attr, 'arc) output
  (** Same as {!FixpointStd.analysis}, but with the technique of
      Gopan and Reps published in Static Anlaysis Symposium,
      SAS'2007. *)

val add_active_hedges :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph -> ('hedge, unit) PHashhe.t -> bool
