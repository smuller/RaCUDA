(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of a dynamically explored equation system *)

open FixpointType

val init :
  ('vertex, 'hedge) SHGraph.compare ->
  ('vertex, 'hedge, 'abs, 'arc) manager ->
  ('vertex, 'hedge) equation -> 'vertex PSette.t ->
  ('vertex, 'hedge, 'abs, 'arc) graph
val propagate :
  guided:bool ->
  ('vertex, 'hedge, 'abs, 'arc) manager ->
  ('vertex, 'hedge, 'abs, 'arc) graph -> bool
val fixpoint :
  guided:bool ->
  ('vertex, 'hedge, 'abs, 'arc) manager ->
  ('vertex, 'hedge, 'abs, 'arc) graph ->
  (('vertex, 'hedge, 'abs, 'arc) graph -> ('vertex, 'hedge) strategy) ->
  bool
val analysis :
  ('vertex, 'hedge) SHGraph.compare ->
  guided:bool ->
  ('vertex, 'hedge, 'abs, 'arc) manager ->
  ('vertex, 'hedge) equation ->  'vertex PSette.t ->
  (('vertex, 'hedge, 'abs, 'arc) graph -> ('vertex, 'hedge) strategy) ->
  ('vertex, 'hedge, 'abs, 'arc) output

val equation_of_graph :
  ?filter:('hedge -> bool) ->
  ('vertex, 'hedge, 'attr, 'arc, 'e) PSHGraph.t ->
  ('vertex, 'hedge) equation

val graph_of_equation :
  ('vertex, 'hedge) SHGraph.compare ->
  ?filter:('hedge -> bool) ->
  make_attrvertex:('vertex -> 'attr) ->
  make_attrhedge:('hedge -> 'arc) ->
  info:'e ->
  ('vertex, 'hedge) equation ->
  'vertex PSette.t ->
  ('vertex, 'hedge, 'attr, 'arc, 'e) PSHGraph.t
