(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system: types *)

(*  ********************************************************************** *)
(** {2 Public datatypes} *)
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

type ('vertex,'hedge,'abstract,'arc) manager = {
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
type strategy_iteration = {
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

type ('vertex,'hedge) strategy_vertex = {
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

val print_strategy_iteration : Format.formatter -> strategy_iteration -> unit

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

val make_strategy_iteration :
  ?widening_start:int -> ?widening_descend:int -> unit -> strategy_iteration
val make_strategy_default :
  ?depth:int ->
  ?widening_start:int ->
  ?widening_descend:int ->
  ?priority:'hedge PSHGraph.priority ->
  vertex_dummy:'vertex ->
  hedge_dummy:'hedge ->
  ('vertex, 'hedge, 'e, 'f, 'g) PSHGraph.t -> 'vertex PSette.t ->
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

(*  ====================================================================== *)
(** {3 Output} *)
(*  ====================================================================== *)

type stat_iteration = {
  mutable nb: int;
  mutable stable: bool;
}
type stat = {
  mutable time : float;
  mutable ascending : (stat_iteration,unit) Ilist.t;
  mutable descending : (stat_iteration,unit) Ilist.t;
}

(** result of the analysis *)
type ('vertex,'hedge,'abstract,'arc) output =
  ('vertex,'hedge,'abstract, 'arc, stat) PSHGraph.t

val ilist_map_condense : ('a -> 'c) -> ('a,'b) Ilist.t -> ('c,'d) Ilist.t
val stat_iteration_merge : (stat_iteration,unit) Ilist.t -> stat_iteration
val print_stat_iteration : Format.formatter -> stat_iteration -> unit
val print_stat_iteration_ilist : Format.formatter -> (stat_iteration,'a) Ilist.t -> unit
val print_stat : Format.formatter -> stat -> unit
    (** Prints statistics *)
val print_output :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  Format.formatter -> ('vertex, 'hedge, 'attr, 'arc) output -> unit
      (** Prints the result of an analysis. *)

(*  ********************************************************************** *)
(** {2 Internal datatypes} *)
(*  ********************************************************************** *)

type 'abstract attr = {
  mutable reach : 'abstract;
  mutable diff : 'abstract;
  mutable empty : bool;
}

type 'arc arc = {
  mutable arc : 'arc;
  mutable aempty : bool;
}

type ('vertex,'hedge) infodyn = {
  mutable iaddhedge : ('hedge,'vertex array * 'vertex) PHashhe.t;
  iequation : ('vertex,'hedge) equation
}

type ('vertex,'hedge) info = {
  iinit : 'vertex PSette.t;
  itime : float ref;
  mutable iascending : (stat_iteration,unit) Ilist.t;
  mutable idescending : (stat_iteration,unit) Ilist.t;
  mutable iworkvertex : ('vertex,unit) PHashhe.t;
  mutable iworkhedge : ('hedge,unit) PHashhe.t;
  iinfodyn : ('vertex,'hedge) infodyn option;
}

type ('vertex,'hedge,'abstract,'arc) graph =
  ('vertex, 'hedge, 'abstract attr, 'arc arc, ('vertex,'hedge) info) PSHGraph.t

val print_attr :
  ('a, 'b, 'c, 'd) manager -> Format.formatter -> 'c attr -> unit
val print_arc :
  ('a, 'b, 'c, 'd) manager -> Format.formatter -> 'd arc -> unit
val print_info :
  ('a, 'b, 'c, 'd) manager -> Format.formatter -> ('a, 'b) info -> unit
val print_workingsets :
  ('a, 'b, 'c, 'd) manager -> Format.formatter -> ('a, 'b, 'c, 'd) graph -> unit

val print_graph :
  ('vertex, 'hedge, 'attr, 'arc) manager ->
  Format.formatter -> ('vertex, 'hedge, 'attr, 'arc) graph -> unit
    (** Prints internal graph. *)

(*  ********************************************************************** *)
(** {2 DOT output} *)
(*  ********************************************************************** *)

val dot_graph :
  ?style:string ->
  ?titlestyle:string ->
  ?vertexstyle:string ->
  ?hedgestyle:string ->
  ?strategy:('a, 'b) strategy ->
  ?vertex:'a ->
  ('a, 'b, 'c, 'd) manager ->
  ('a, 'b, 'c, 'd) graph -> title:string -> unit
    (** Prints internal graph on the (optional) formatter
	[man.dot_fmt], see type {!manager}. *)
