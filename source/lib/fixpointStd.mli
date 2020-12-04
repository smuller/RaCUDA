(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system: standard method *)

open FixpointType

(*  ********************************************************************** *)
(** {2 Utilities (internal functions)} *)
(*  ********************************************************************** *)

val is_tvertex :
  ('vertex, 'hedge, 'abstract, 'arc) graph ->
  'vertex array -> bool
    (** Does the array of vertices belong to the graph, all with
	non bottom values ? *)

val treach_of_tvertex :
  descend:bool ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  'vertex array -> 'attr array
    (** Return the array of abstract values associated to the
	vertices *)

val update_workingsets :
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  hedge:bool ->
  'vertex -> unit
    (** Update working sets assuming that the abstract value
	associated to the vertex has been modified. If
	[hedge=true], then also consider the working set
	associated to hyperhedges. *)

(*  ********************************************************************** *)
(** {2 Initialisation of fixpoint computation} *)
(*  ********************************************************************** *)

val init :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
  ('vertex, 'hedge, 'attr, 'arc) graph
(** [init manager input sinit] creates the internal graph
    structure (from the equation graph [input]) and initialize the
    working sets (from the set of initial points [sinit]) (stored
    in the info field of the internal graph). *)

(*  ********************************************************************** *)
(** {2 Process a vertex (internal functions)} *)
(*  ********************************************************************** *)

val accumulate_vertex :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  ('vertex, 'hedge) strategy_vertex -> 'attr attr -> bool
    (** Compute the least upper bound of the current value of the
	vertex/variable with the values propagated by the incoming
	hyperedges belonging to the working set. Returns [true] if
	the value is strictly increasing. *)

val propagate_vertex :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  descend:bool -> ('vertex, 'hedge) strategy_vertex -> 'attr attr -> bool

val process_vertex :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  widening:bool -> ('vertex, 'hedge) strategy_vertex -> bool

(*  ********************************************************************** *)
(** {2 Full fixpoint algorithm} *)
(*  ********************************************************************** *)

val process_strategy :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  depth:int -> ('vertex, 'hedge) strategy -> bool
val descend_strategy :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph -> ('vertex, 'hedge) strategy -> bool
    (** Internal functions *)
val descend :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph -> ('vertex, 'hedge) strategy -> bool
    (** (Rather internal)

	[descend manager graph strategy] performs descending
	iterations on the part of the graph represented by the
	strategy *)

val process_toplevel_strategy :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'attr, 'arc) graph ->
  ('vertex, 'hedge) strategy -> bool * bool
    (** (Rather internal function)

	[process_toplevel_strategy manager graph strategy]:
	assuming that [graph] has been created with the function
	{!init}, this function solves iteratively the fixpoint
	equation, using [manager] for interpreting the equation
	system [graph], and the strategy [strategy] for the
	iteration order and the application of widening.

	Descending iterations are applied separately to each
	upper-level component of the strategy, before processing
	the next such component in the strategy.

	The first returned Boolean indicates if some growth has
	been observed (before descending iteration), and the
	second one indicates if some reduction has been observed
	after descending iteration. *)

val output_of_graph :
  ('vertex, 'hedge, 'abstract,'arc) graph ->
  ('vertex, 'hedge, 'abstract,'arc) output
    (** (Rather internal function)

	Getting the result of the analysis from the internal
	representation. *)

val analysis :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
  ('vertex, 'hedge) strategy ->
  ('vertex, 'hedge, 'attr, 'arc) output
    (** Main user function: [analysis manager equation_graph sinit
	strategy] takes a graph giving the structure of the equation
	system, a manager indicating how to interpret the equation
	system, a (super)set of the variables to be initialized to a
	non-empty value, and an iteration strategy. It returns the
	result of the full analysis with an object of type
	{! Fixpoint.output}. *)
