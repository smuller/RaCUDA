(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system *)

(*  ********************************************************************** *)
(** {2 Datatypes} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Manager} *)
(*  ====================================================================== *)

(** The manager parameterizes the fixpoint solver with the
    following types:
    {ul
    {- ['vertex]: type of vertex/variable (identifier) in the hypergraph
    describing the equation system;}
    {- ['hedge]: type of hyperedge/function (identifier) in the hypergraph
    describing the equation system;}
    {- ['abstract]: type of abstract values associated to vertices/variables;}
    {- ['arc]: type of information associated to hyperedges/functions;}}
    and values:
    {ul
    {- Lattice operations on abstract values; the argument of type ['vertex]
      indicates to which vertex the result of the function is
      associated (useful when abstract values are typed by an
      environment, for instance)}
    {- Functions to initialize variables and to interpret hyperedges/functions;}
    {- Printing functions for type parameters;}
    {- Options, mainly about widening;}
    {- Debugging options, for text (and possibly DOT) output on the formatter [print_fmt];}
    {- Printing functions for DOT output on the optional formatter [dot_fmt].}
    }
*)

type ('vertex,'hedge,'abstract,'arc) manager =
  ('vertex,'hedge,'abstract,'arc) FixpointType.manager = {
  mutable bottom : 'vertex -> 'abstract;
      (** Create a bottom value *)
  mutable canonical : 'vertex -> 'abstract -> unit;
      (** Make an abstract value canonical *)
  mutable is_bottom : 'vertex -> 'abstract -> bool;
      (** Emptiness test *)
  mutable is_leq : 'vertex -> 'abstract -> 'abstract -> bool;
      (** Inclusion test *)
  mutable join :  'vertex -> 'abstract -> 'abstract -> 'abstract;
  mutable join_list : 'vertex -> 'abstract list -> 'abstract;
      (** Binary and n-ary join operation *)
  mutable widening : 'vertex -> 'abstract -> 'abstract -> 'abstract;
      (** Apply widening at the given point, with the two arguments.
	  Widening will always be applied with first argument being included
	  in the second one. *)
  mutable odiff :  ('vertex -> 'abstract -> 'abstract -> 'abstract) option;
      (** Sound approximation of set difference (optional) *)
  mutable abstract_init : 'vertex -> 'abstract;
      (** Return the non-bottom initial value associated to the given vertex *)
  mutable arc_init : 'hedge -> 'arc;
      (** Initial value for arcs *)
  mutable apply : 'hedge -> 'abstract array -> 'arc * 'abstract;
      (** Apply the function indexed by [hedge] to the array of arguments.

	  It returns the new abstract value, but also a user-defined information
	  that will be associated to the hyperedge in the result. *)

  mutable print_vertex : Format.formatter -> 'vertex -> unit;
  mutable print_hedge : Format.formatter -> 'hedge -> unit;
  mutable print_abstract: Format.formatter -> 'abstract -> unit;
  mutable print_arc: Format.formatter -> 'arc -> unit;
      (** Printing functions *)

  mutable accumulate : bool;
	(** If true, during ascending phase, compute the union of old reachable
	    value with growing incoming hyperedges. If false, recompute all
	    incoming hyperedges. *)

  mutable print_fmt : Format.formatter;
	(** Typically equal to [Format.std_formatter] *)
  mutable print_analysis : bool;
  mutable print_component : bool;
  mutable print_step : bool;
  mutable print_state : bool;
  mutable print_postpre : bool;
  mutable print_workingsets : bool;
      (** Printing Options *)

  mutable dot_fmt : Format.formatter option;
	(** [Some fmt] enables DOT output. You can set dummy
	    values to the fields below if you always set [None]
	    and you do not want DOT output. *)
  mutable dot_vertex : Format.formatter -> 'vertex -> unit;
      (** Print vertex identifiers in DOT format *)
  mutable dot_hedge : Format.formatter -> 'hedge -> unit;
      (** Print hyperedge identifiers in DOT format (vertices and
	  hyperedges identifiers should be different, as they are
	  represented by DOT vertices *)
  mutable dot_attrvertex : Format.formatter -> 'vertex -> unit;
      (** Print the displayed information in boxes *)
  mutable dot_attrhedge : Format.formatter -> 'hedge -> unit;
      (** Print the displayed information for hyperedges *)
}

(*  ====================================================================== *)
(** {3 Static equation system} *)
(*  ====================================================================== *)

(** A static equation system is defined by an hypergraph, of type
    {!PSHGraph.t}, see {!analysis_std} and {!analysis_guided} *)

(*  ====================================================================== *)
(** {3 Dynamically explored equation system} *)
(*  ====================================================================== *)

type ('vertex,'hedge) equation =
  'vertex -> ('hedge, 'vertex array * 'vertex) PMappe.t
    (** Function that explores dynamically an equation system.
	[equation vertex] returns a map hat associates to each
	successor hyperedge a pair of composed of the set of
	predecessor vertices, and the successor vertex.  *)

(*  ====================================================================== *)
(** {3 Iteration strategies} *)
(*  ====================================================================== *)

(** Widening and Descending Options *)
type strategy_iteration = FixpointType.strategy_iteration = {
  mutable widening_start : int;
    (** Nb of initial steps without widening in the current strategy *)
  mutable widening_descend : int;
    (** Maximum nb. of descending steps in the current strategy *)
  mutable ascending_nb : int;
    (** For stats *)
  mutable descending_nb : int;
    (** For stats *)
  mutable descending_stable : bool;
    (** For stats *)
}

type ('vertex,'hedge) strategy_vertex =
  ('vertex,'hedge) FixpointType.strategy_vertex = {
  mutable vertex : 'vertex;
  mutable hedges : 'hedge list;
      (** Order in which the incoming hyperedges will be applied *)
  mutable widen : bool;
      (** Should this vertex be a widening point ? *)
}
  (** Strategy to be applied for the vertex [vertex].
    - [hedges] is a list of incoming hyperedges. The effect of hyperedges are
      applied "in parallel" and the destination vertex is updated. Be cautious:
      if an incoming hyperedge is forgotten in this list, it won't be taken
      into account in the analysis.
    - [widen] specifies whether the vertex is a widening point or not.  *)

type ('vertex,'hedge) strategy =
    (strategy_iteration, ('vertex,'hedge) strategy_vertex) Ilist.t
    (** Type for defining iteration strategies.  For instance, [[1; [2;3]; 4;
      [5]; 6]] means:
      - update [1];
      - update [2] then [3], and loop until stabilization;
      - update [4];
      - update [5] and loop until stabilization;
      - update [6] and ends the analysis.

	Moreover, to each (imbricated) list is associated a record
	of type [strategy_iteration], which indicates when to
	start the widening, and the maximum number of descending
	iterations.

	Some observations on this example:

	- The user should specify correctly the strategy.  Two
	vertices belonging to the same connex component should
	always belong to a loop. Here, if there is an edge from
	[6] to [2], the loop will not be iterated.
	- A vertex may appear more than once in the strategy, if it is useful.
	- Definition of the set of widening point is independent from the order
	- of application, here. it is alos the user-responsability to ensure that
	- the computation will end.

	So-called stabilization loops can be recursive, like that: [[1; [2;
	[3;4]; [5]]; 6]], where the loop [[3;4]] needs to be (temporarily stable)
	before going on with [5].

    *)

(*  ====================================================================== *)
(** {3 Output} *)
(*  ====================================================================== *)

type stat_iteration = FixpointType.stat_iteration = {
  mutable nb: int;
  mutable stable: bool;
}
type stat = FixpointType.stat = {
  mutable time : float;
  mutable ascending : (stat_iteration,unit) Ilist.t;
  mutable descending : (stat_iteration,unit) Ilist.t;
}
  (** statistics at the end of the analysis *)

type ('vertex,'hedge,'abstract,'arc) output =
    ('vertex,'hedge,'abstract,'arc, stat) PSHGraph.t
      (** result of the analysis *)

(*  ********************************************************************** *)
(** {2 Functions} *)
(*  ********************************************************************** *)

val make_strategy_default :
  ?depth:int ->
  ?widening_start:int ->
  ?widening_descend:int ->
  ?priority:'hedge PSHGraph.priority ->
  vertex_dummy:'vertex ->
  hedge_dummy:'hedge ->
  ('vertex, 'hedge, 'e,'f,'g) PSHGraph.t -> 'vertex PSette.t ->
  ('vertex, 'hedge) strategy
    (**

       Build a "default" strategy, with the following options:

       - depth: to apply the recursive strategy of Bourdoncle's paper.  Default
       value is 2, which means that Strongly Connected Components are
       stabilized independently: the iteration order looks like [(1 2 (3 4 5)
       6 (7 8) 9)] where [1, 2, 6, 9] are SCC by themselves. A higher value
       defines a more recursive behavior, like [(1 2 (3 (4 5)) 6 (7 (8)) 9)].

       - iteration: for each strategy, nb if initial steps without
       widening and max nb. of descending steps (the latter being used only for
       depth 2). Default is [(0,1)].

       - priority: specify which hedges should be taken into account in the
       computation of the iteration order and the widening points (the one
       such that [priority h >= 0] and the widening points, and also indicates
       which hyperedge should be explored first at a point of choice.

       One known usage for filtering: guided analysis, where one analyse
       a subgraph of the equation graph.
    *)

val analysis_std :
  ('vertex, 'hedge, 'abstract, 'arc) manager ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
  ('vertex, 'hedge) strategy ->
  ('vertex, 'hedge, 'abstract,'arc) output
    (** Performs initialization, fixpoint analysis and descending,
	and measures the global analysis time.

	[analysis_std manager graph sinit strategy] takes a graph
	giving the structure of the equation system, a manager
	indicating how to interpret the equation system, a
	(super)set [sinit] of the variables to be initialized to a
	non-empty value, and an iteration strategy [strategy].
    *)

val analysis_guided :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
  (('hedge -> bool) -> ('vertex,'hedge) strategy) ->
  ('vertex, 'hedge, 'attr, 'arc) output
    (** Same as {!analysis_std}, but with the technique of
	Gopan and Reps published in Static Anlaysis Symposium,
	SAS'2007.

	[analysis_guided manager graph sinit make_strategy]:
	compared to {!analysis_std}, instead of providing a
	strategy, one provides a function [make_strategy]
	generating strategies, which takes as input a function
	filtering the edges to be considered. A typical value for
	the argument [make_strategy] is [(fun p ->
	make_strategy_default ~priority:(PSHGraph.Filter p) vdummy
	hdummy graph sinit)].  *)

val equation_of_graph :
  ?filter:('hedge -> bool) ->
  ('vertex, 'hedge, 'attr, 'arc, 'e) PSHGraph.t ->
  ('vertex, 'hedge) equation
    (** Generate from a graph a function of type [('vertex,
	'hedge) equation] or dynamically exploring the graph.  The
	[filter] function allows to select a part of the graph. *)

val graph_of_equation :
  ('vertex, 'hedge) PSHGraph.compare ->
  ?filter:('hedge -> bool) ->
  make_attrvertex:('vertex -> 'attr) ->
  make_attrhedge:('hedge -> 'arc) ->
  info:'e ->
  ('vertex, 'hedge) equation ->
  'vertex PSette.t -> ('vertex, 'hedge, 'attr, 'arc, 'e) PSHGraph.t
  (** Generate from an equation a graph, using
      [make_attrvertex], [make_attrhedge] and [info]. *)

val analysis_dyn :
  ('a, 'b) SHGraph.compare ->
  guided:bool ->
  ('a, 'b, 'c, 'd) manager ->
  ('a, 'b) equation -> 'a PSette.t ->
  (('a, 'b, 'c, 'd) FixpointType.graph -> ('a, 'b) strategy) ->
  ('a, 'b, 'c, 'd) output
    (** Dynamic analysis. *)

(*  ********************************************************************** *)
(** {2 Printing Functions} *)
(*  ********************************************************************** *)

val print_strategy_vertex :
  ('a, 'b, 'c, 'd) manager ->
  Format.formatter -> ('a, 'b) strategy_vertex -> unit
  (** [print_strategy_vertex man fmt sv] prints an object of type
      [strategy_vertex], using the manager [man] for printing vertices and
      hyperedges.  The output has the form [(boolean,vertex,[list of list of
      hedges])].  *)
val print_strategy :
  ('a, 'b, 'c, 'd) manager -> Format.formatter -> ('a, 'b) strategy -> unit
  (** [print_strategy_vertex man fmt sv] prints an object of type
      [strategy], using the manager [man] for printing vertices and
      hyperedges. *)
val print_stat : Format.formatter -> stat -> unit
  (** Prints statistics *)
val print_output :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  Format.formatter -> ('vertex, 'hedge, 'attr, 'arc) output -> unit
      (** Prints the result of an analysis. *)
