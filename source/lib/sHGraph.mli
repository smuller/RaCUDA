(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Oriented hypergraphs *)

(** {2 Introduction}

This module provides an abstract datatypes and functions for manipulating
hypergraphs, that is, graphs where edges relates potentially more than 2
vertices. The considered hypergraphs are {e oriented}: one distinguishes for a
vertex incoming and outgoing hyperedges, and for an hyperedge incoming (or
origin) and outgoing (or destination) vertices.

Origin and destination vertices of an hyperedge are ordered (by using arrays),
in contrast with incoming and outgoing hyperedges of a vertex.

A possible use of such hypergraphs is the representation of a (fixpoint)
equation system, where the unknown are the vertices and the functions the
hyperedges, taking a vector of unknowns as arguments and delivering a vector of
results.

A last note about the notion of connectivity, which is relevant for operations
like depth-first-search, reachability and connex components notions. A
destination vertex of an hyperedge is considered as reachable from an origin
vertex through this hyperedge only if {e all} origin vertices are reachable.

*)

type 'a priority =
  | Filter of ('a -> bool)
  | Priority of ('a -> int)
(** Filtering or priority function (used by {!topological_sort}, {!cfc},
  {!scfc}, {!topological_sort_multi}, {!cfc_multi}, {!scfc_multi}).

  [Filter p] specifies [p] as a filtering function for hyperedges: only those
  satisfying [p] are taken into account.

  [Priority p] specifies [p] as a priority function. Hyperedges [h] with [p h <
  0] are not taken into account. Otherwise, hyperedges with highest priority
  are explored first.
*)

type ('a,'b) compare = {
  hashv : 'a Hashhe.compare;
  hashh : 'b Hashhe.compare;
  comparev : 'a -> 'a -> int;
  compareh : 'b -> 'b -> int;
}
type ('b,'c) vertex_n = {
  attrvertex : 'c;
  mutable predhedge : 'b Sette.set;
  mutable succhedge : 'b Sette.set;
}
type ('a,'d) hedge_n = {
  attrhedge : 'd;
  predvertex : 'a array;
  succvertex : 'a array;
}
type ('a,'b, 'c, 'd, 'e) graph = {
  vertex : ('a, ('b,'c) vertex_n) Hashhe.hashtbl;
  hedge : ('b, ('a,'d) hedge_n) Hashhe.hashtbl;
  info : 'e
}

(*  ********************************************************************** *)
(** {2 Generic (polymorphic) interface} *)
(*  ********************************************************************** *)

val stdcompare : ('a,'b) compare

type ('a,'b,'c,'d, 'e) t = ('a,'b, 'c, 'd, 'e) graph
  (** The type of hypergraphs where {ul
      {- 'a : type of vertices}
      {- 'b : type of hedges}
      {- 'c : information associated to vertices}
      {- 'd : information associated to hedges}
      {- 'e : user-information associated to an hypergraph}
      }
  *)
val create : int -> 'e -> ('a,'b,'c,'d,'e) t
  (** [create n data] creates an hypergraph, using [n] for the initial size
    of internal hashtables, and [data] for the user information *)

val clear : ('a,'b,'c,'d,'e) t -> unit
  (** Remove all vertices and hyperedges of the graph. *)

val is_empty : ('a,'b,'c,'d,'e) t -> bool
  (** Is the graph empty ? *)

(*  ====================================================================== *)
(** {3 Statistics} *)
(*  ====================================================================== *)

val size_vertex : ('a,'b,'c,'d,'e) t -> int
  (** Number of vertices in the hypergraph *)
val size_hedge : ('a,'b,'c,'d,'e) t -> int
  (** Number of hyperedges in the hypergraph *)
val size_edgevh : ('a,'b,'c,'d,'e) t -> int
  (** Number of edges (vertex,hyperedge) in the hypergraph *)
val size_edgehv : ('a,'b,'c,'d,'e) t -> int
  (** Number of edges (hyperedge,vertex) in the hypergraph *)
val size : ('a,'b,'c,'d,'e) t -> int * int * int * int
  (** [size graph] returns [(nbvertex,nbhedge,nbedgevh,nbedgehv)] *)

(*  ====================================================================== *)
(** {3 Information associated to vertives and edges} *)
(*  ====================================================================== *)

val attrvertex : ('a,'b,'c,'d,'e) t -> 'a -> 'c
  (** [attrvertex graph vertex] returns the information associated to the
    vertex [vertex] *)
val attrhedge : ('a,'b,'c,'d,'e) t -> 'b -> 'd
  (** [attrhedge graph hedge] returns the information associated to the
    hyperedge [hedge] *)
val info : ('a,'b,'c,'d,'e) t -> 'e
  (** [info g] returns the user-information attached to the graph [g] *)

(** {3 Membership tests} *)

val is_vertex : ('a,'b,'c,'d,'e) t -> 'a -> bool
val is_hedge : ('a,'b,'c,'d,'e) t -> 'b -> bool

(*  ====================================================================== *)
(** {3 Successors and predecessors} *)
(*  ====================================================================== *)

val succhedge : ('a,'b,'c,'d,'e) t -> 'a -> 'b Sette.t
  (** Successor hyperedges of a vertex *)
val predhedge : ('a,'b,'c,'d,'e) t -> 'a -> 'b Sette.t
  (** Predecessor hyperedges of a vertex *)
val succvertex : ('a,'b,'c,'d,'e) t -> 'b -> 'a array
  (** Successor vertices of an hyperedge *)
val predvertex : ('a,'b,'c,'d,'e) t -> 'b -> 'a array
  (** Predecessor vertices of an hyperedge *)
val succ_vertex : ('a,'b,'c,'d,'e) t -> 'a -> 'a Sette.t
  (** Successor vertices of a vertex by any hyperedge *)
val pred_vertex : ('a,'b,'c,'d,'e) t -> 'a -> 'a Sette.t
  (** Predecessor vertices of a vertex by any hyperedge *)

(*  ====================================================================== *)
(** {3 Adding and removing elements} *)
(*  ====================================================================== *)

val add_vertex : ('a,'b,'c,'d,'e) t -> 'a -> 'c -> unit
  (** Add a vertex *)
val add_hedge : ('a,'b,'c,'d,'e) t -> 'b -> 'd -> pred:'a array -> succ:'a array -> unit
  (** Add an hyperedge. The predecessor and successor vertices
      should already exist in the graph. Otherwise, a [Failure]
      exception is raised. *)
val replace_attrvertex : ('a,'b,'c,'d,'e) t -> 'a -> 'c -> unit
  (** Change the attribute of an existing vertex *)
val replace_attrhedge : ('a,'b,'c,'d,'e) t -> 'b -> 'd -> unit
  (** Change the attribute of an existing hyperedge *)
val remove_vertex : ('a,'b,'c,'d,'e) t -> 'a -> unit
  (** Remove the vertex from the graph, as well as all related hyperedges. *)
val remove_hedge : ('a,'b,'c,'d,'e) t -> 'b -> unit
  (** Remove the hyperedge from the graph. *)

(*  ====================================================================== *)
(** {3 Iterators} *)
(*  ====================================================================== *)

val iter_vertex :
  ('a,'b,'c,'d,'e) t ->
  ('a -> 'c -> pred:'b Sette.t -> succ:'b Sette.t -> unit) ->
  unit
  (** Iterates the function [f vertex attrvertex succhedges predhedges] to all
    vertices of the graph.  [succhedges] (resp. [predhedges]) is the set of
    successor (resp. predecessor) hyperedges of the vertex *)
val iter_hedge :
  ('a,'b,'c,'d,'e) t ->
  ('b -> 'd -> pred:'a array -> succ:'a array -> unit) ->
  unit
  (** Iterates the function [f hedge attrhedge succvertices predvertices] to
    all hyperedges of the graph.  [succvertices] (resp. [predvertices]) is the
    set of successor (resp. predecessor) vertices of the hyperedge *)

(** Below are the [fold] versions of the previous functions. *)

val fold_vertex :
  ('a,'b,'c,'d,'e) t ->
  ('a -> 'c -> pred:'b Sette.t -> succ:'b Sette.t -> 'h -> 'h) ->
  'h -> 'h
val fold_hedge :
  ('a,'b,'c,'d,'e) t ->
  ('b -> 'd ->  pred:'a array -> succ:'a array -> 'h -> 'h) ->
  'h -> 'h

(** Below are the [map] versions of the previous functions. *)

val map :
  ('a,'b,'c,'d,'e) t ->
  ('a -> 'c -> 'cc) ->
  ('b -> 'd -> 'dd) ->
  ('e -> 'ee) ->
  ('a,'b,'cc,'dd,'ee) t

(*  ====================================================================== *)
(** {3 Copy and Transpose} *)
(*  ====================================================================== *)

val copy :
  ('a -> 'c -> 'cc) ->
  ('b -> 'd -> 'dd) ->
  ('e -> 'ee) ->
  ('a,'b,'c,'d,'e) t ->
  ('a,'b,'cc,'dd,'ee) t
(** Copy an hypergraph, using the given functions to duplicate the
  attributes associated to the elements of the graph. The vertex and
  hedge identifiers are copied using the identity function. *)

val transpose :
  ('a -> 'c -> 'cc) ->
  ('b -> 'd -> 'dd) ->
  ('e -> 'ee) ->
  ('a,'b,'c,'d,'e) t ->
  ('a,'b,'cc,'dd,'ee) t
(** Similar to [copy], but hyperedges are reversed: successor vertices
  and predecessor vertices are exchanged. *)

(*  ====================================================================== *)
(** {3 Algorithms} *)
(*  ====================================================================== *)

val min : ('a,'b,'c,'d,'e) t -> 'a Sette.t
  (** Return the set of vertices without predecessor hyperedges *)
val max : ('a,'b,'c,'d,'e) t -> 'a Sette.t
  (** Return the set of vertices without successor hyperedges *)

(*  ---------------------------------------------------------------------- *)
(** {4 Topological sort} *)
(*  ---------------------------------------------------------------------- *)

val topological_sort :
  ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a -> 'a list
  (** Topological sort of the vertices of the hypergraph starting from
    a root vertex. The graph supposed to be acyclic. Any hyperedge
    linking two vertices (which are resp. predecessor and successor)
    induces a dependency. The result contains only vertices reachable
    from the given root vertex.  If the dependencies are cyclic, the
    result is meaningless. *)
val topological_sort_multi :
  'a -> 'b -> ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a Sette.t -> 'a list
  (** Topological sort from a set of root vertices.  The two first
    arguments are supposed to be yet unused vertex and hyperedge
    identifier.*)

(*  ---------------------------------------------------------------------- *)
(** {4 Reachability and coreachability} *)
(*  ---------------------------------------------------------------------- *)

(** The variants of the basic functions are similar to the variants
 described above. *)

val reachable :
 ?filter:('b -> bool) ->
 ('a,'b,'c,'d,'e) t -> 'a -> 'a Sette.t * 'b Sette.t
  (** Returns the set of vertices and hyperedges that are *NOT* reachable from
    the given root vertex. Any dependency in the sense described above is taken
    into account to define the reachability relation. For instance, if one of
    the predecessor vertex of an hyperedge is reachable, the hyperedge is
    considered as reachable. *)
val reachable_multi :
  'a -> 'b ->  ?filter:('b -> bool) ->
  ('a,'b,'c,'d,'e) t -> 'a Sette.t -> 'a Sette.t * 'b Sette.t
  (* Reachability from a set of root vertices *)

(*  ---------------------------------------------------------------------- *)
(** {4 Strongly Connected Components and SubComponents} *)
(*  ---------------------------------------------------------------------- *)

val cfc :
  ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a -> 'a list list
  (** Decomposition of the graph into Strongly Connected Components,

    [cfc graph vertex] returns a decomposition of the graph.  The exploration
    is done from the initial vertex [vertex], and only reachable vertices are
    included in the result. The result has the structure [[comp1 comp2 comp3
    ...]] where each component is defined by a list of vertices. The ordering
    of component correspond to a linearization of the partial order between the
    components.  *)

val cfc_multi :
  'a -> 'b -> ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a Sette.t -> 'a list list
  (** idem, but from several initial vertices.

    [cfc dummy_vertex dummy_hedge graph setvertices] returns a decomposition of
    the graph, explored from the set of initial vertices
    [setvertices]. [dummy_vertex] and [dummy_hedge] are resp. unused vertex and
    hyperedge identifiers.
  *)

val scfc :
  ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a -> (unit,'a) Ilist.t
  (** Decomposition of the graph into Strongly Connected Sub-Components,

    [scfc graph vertex] returns a decomposition of the graph.  The exploration
    is done from the initial vertex [vertex], and only reachable vertices are
    included in the result. The result has the structure [[comp1 comp2 comp3
    ...]] where each component is in turn decomposed into components.
    The third parameter can be used to assign a value to each component (sub-list). *)
val scfc_multi :
  'a -> 'b -> ?priority:'b priority ->
  ('a,'b,'c,'d,'e) t -> 'a Sette.t -> (unit,'a) Ilist.t
  (** idem, but from several initial vertices. *)

(*  ====================================================================== *)
(** {3 Printing} *)
(*  ====================================================================== *)

val print :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter -> 'e -> unit) ->
  Format.formatter -> ('a,'b,'c,'d,'e) t -> unit
  (** Print a graph in textual format on the given formatter, using the given
    functions to resp.  print: vertices (['a]), hedges (['b]), vertex
    attributes (['c]), hedge attributes (['d]), and the user information
    (['e]). *)

val print_dot :
  ?style:string ->
  ?titlestyle:string ->
  ?vertexstyle:string ->
  ?hedgestyle:string ->
  ?fvertexstyle:('a -> string) ->
  ?fhedgestyle:('b -> string) ->
  ?title:string ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'a -> 'c -> unit) ->
  (Format.formatter -> 'b -> 'd -> unit) ->
  Format.formatter -> ('a,'b,'c,'d,'e) t -> unit
  (** Output the graph in DOT format on the given formatter, using the given
    functions to resp print:

    {ul {li vertex identifiers (in the DOT file)} {li hedge identifiers (in the
    DOT file).

      BE CAUTIOUS. as the DOT files vertices and hedges are actually nodes, the
      user should take care to avoid name conflicts between vertex and hedge
      names.}  {li vertex attributes.

      BE CAUTIOUS: the output of the function will be enclosed bewteen
      quotes. If ever the output contains line break, or other special
      characters, it should be escaped. A possible scheme to do this is to
      first output to [Format.str_formatter] with a standard printing function,
      then to escape the resulting string and to output the result.  This gives
      something like:

      [print_attrvertex Format.str_formatter vertex attr;]

      [Format.pp_print_string fmt (String.escaped (Format.flush_str_formatter
      ()));].

      Concerning the escape function, you may use [String.escaped], which will
      produce center justified line breaks, or [Print.escaped] which allows
      also to choose between center, left and right justified lines.}  {li
      hedge atributes (same comment as for vertex attributes).}}

    The optional arguments allows to customize the style. The default setting
    corresponds to:

    [print_dot ~style="ranksep=0.1; size=\"7,10\";" ~titlestyle="shape=ellipse,style=bold,style=filled,fontsize=20"
~vertexstyle="shape=box,fontsize=12" ~hedgestyle="shape=ellipse,fontsize=12"
~title="" ...].  *)

(*  ********************************************************************** *)
(** {2 Parameter module for the functor version} *)
(*  ********************************************************************** *)

module type T = sig
  type vertex
    (** Type of vertex identifiers *)
  type hedge
    (** Type of hyperedge identifiers *)
  val vertex_dummy : vertex
    (** A dummy (never used) value for vertex identifiers (used for the
      functions [XXX_multi]) *)
  val hedge_dummy : hedge
    (** A dummy (never used) value for hyperedge identifiers (used for
      the functions [XXX_multi]) *)
  module SetV : (Sette.S with type elt=vertex)
    (** Set module for vertices *)
  module SetH : (Sette.S with type elt=hedge)
    (** Set module for hyperedges *)
  module HashV : (Hashhe.S with type key=vertex)
    (** Hash module with vertices as keys *)
  module HashH : (Hashhe.S with type key=hedge)
    (** Hash module with hyperedges as keys *)
end

(*  ********************************************************************** *)
(** {2 Signature of the functor version} *)
(*  ********************************************************************** *)

(** All functions have the same signature as the polymorphic version,
  except the functions [XXX_multi] which does not need any more a dummy
  value of type [vertex] (resp. [hedge]). *)

module type S = sig
  type vertex
  type hedge
  val vertex_dummy : vertex
  val hedge_dummy : hedge

  module SetV : (Sette.S with type elt=vertex)
  module SetH : (Sette.S with type elt=hedge)
  module HashV : (Hashhe.S with type key=vertex)
  module HashH : (Hashhe.S with type key=hedge)

  val stdcompare : (vertex,hedge) compare
  type ('a,'b,'c) t
    (** Type of hypergraphs, where {ul
    {- 'a : information associated to vertices}
    {- 'b : information associated to hedges}
    {- 'c : user-information associated to an hypergraph}
    }
    *)
  val create : int -> 'c -> ('a,'b,'c) t
  val clear : ('a,'b,'c) t -> unit
  val is_empty : ('a,'b,'c) t -> bool

  (** {3 Statistics} *)

  val size_vertex : ('a,'b,'c) t -> int
  val size_hedge : ('a,'b,'c) t -> int
  val size_edgevh : ('a,'b,'c) t -> int
  val size_edgehv : ('a,'b,'c) t -> int
  val size : ('a,'b,'c) t -> int * int * int * int

  (** {3 Information associated to vertives and edges} *)

  val attrvertex : ('a,'b,'c) t -> vertex -> 'a
  val attrhedge : ('a,'b,'c) t -> hedge -> 'b
  val info : ('a,'b,'c) t -> 'c

  (** {3 Membership tests} *)

  val is_vertex : ('a,'b,'c) t -> vertex -> bool
  val is_hedge : ('a,'b,'c) t -> hedge -> bool

  (** {3 Successors and predecessors} *)

  val succhedge : ('a,'b,'c) t -> vertex -> SetH.t
  val predhedge : ('a,'b,'c) t -> vertex -> SetH.t
  val succvertex : ('a,'b,'c) t -> hedge -> vertex array
  val predvertex : ('a,'b,'c) t -> hedge -> vertex array
  val succ_vertex : ('a,'b,'c) t -> vertex -> SetV.t
  val pred_vertex : ('a,'b,'c) t -> vertex -> SetV.t

  (** {3 Adding and removing elements} *)

  val add_vertex : ('a,'b,'c) t -> vertex -> 'a -> unit
  val add_hedge : ('a,'b,'c) t -> hedge -> 'b -> pred:vertex array -> succ:vertex array -> unit
  val replace_attrvertex : ('a,'b,'c) t -> vertex -> 'a -> unit
  val replace_attrhedge : ('a,'b,'c) t -> hedge -> 'b -> unit
  val remove_vertex : ('a,'b,'c) t -> vertex -> unit
  val remove_hedge : ('a,'b,'c) t -> hedge -> unit

  (** {3 Iterators} *)

  val iter_vertex :
    ('a,'b,'c) t ->
    (vertex -> 'a -> pred:SetH.t -> succ:SetH.t -> unit) ->
    unit
  val iter_hedge :
    ('a,'b,'c) t ->
    (hedge -> 'b -> pred:vertex array -> succ:vertex array -> unit) ->
    unit
  val fold_vertex :
    ('a,'b,'c) t ->
    (vertex -> 'a -> pred:SetH.t -> succ:SetH.t -> 'g -> 'g) ->
    'g -> 'g
  val fold_hedge :
    ('a,'b,'c) t ->
    (hedge -> 'b -> pred:vertex array -> succ:vertex array -> 'g -> 'g) ->
    'g -> 'g

  (** {3 Copy and Transpose} *)

  val map :
    ('a,'b,'c) t ->
    (vertex -> 'a -> 'aa) ->
    (hedge -> 'b -> 'bb) ->
    ('c -> 'cc) ->
    ('aa,'bb,'cc) t

  val copy :
    (vertex -> 'a -> 'aa) ->
    (hedge -> 'b -> 'bb) ->
    ('c -> 'cc) ->
    ('a,'b,'c) t ->
    ('aa,'bb,'cc) t
  val transpose :
    (vertex -> 'a -> 'aa) ->
    (hedge -> 'b -> 'bb) ->
    ('c -> 'cc) ->
    ('a,'b,'c) t ->
    ('aa,'bb,'cc) t

  (** {3 Algorithms} *)

  val min : ('a,'b,'c) t -> SetV.t
  val max : ('a,'b,'c) t -> SetV.t

  val topological_sort :
    ?priority:hedge priority -> ('a,'b,'c) t -> vertex -> vertex list
  val topological_sort_multi :
    ?priority:hedge priority -> ('a,'b,'c) t -> SetV.t -> vertex list

  val reachable :
    ?filter:(hedge -> bool) -> ('a,'b,'c) t -> vertex -> SetV.t * SetH.t
  val reachable_multi :
    ?filter:(hedge -> bool) -> ('a,'b,'c) t -> SetV.t -> SetV.t * SetH.t

  val cfc :
    ?priority:hedge priority -> ('a,'b,'c) t -> vertex -> vertex list list
  val cfc_multi :
    ?priority:hedge priority -> ('a,'b,'c) t -> SetV.t -> vertex list list

  val scfc :
    ?priority:hedge priority -> ('a,'b,'c) t -> vertex -> (unit,vertex) Ilist.t
  val scfc_multi :
    ?priority:hedge priority -> ('a,'b,'c) t -> SetV.t -> (unit,vertex) Ilist.t

  (** {3 Printing} *)

  val print :
    (Format.formatter -> vertex -> unit) ->
    (Format.formatter -> hedge -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    Format.formatter -> ('a,'b,'c) t -> unit

  val print_dot :
    ?style:string ->
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?hedgestyle:string ->
    ?fvertexstyle:(vertex -> string) ->
    ?fhedgestyle:(hedge -> string) ->
    ?title:string ->
    (Format.formatter -> vertex -> unit) ->
    (Format.formatter -> hedge -> unit) ->
    (Format.formatter -> vertex -> 'a -> unit) ->
    (Format.formatter -> hedge -> 'b -> unit) ->
    Format.formatter -> ('a,'b,'c) t -> unit

end

(*  ********************************************************************** *)
(** {2 Functor} *)
(*  ********************************************************************** *)

module Make(T : T) : (S with type vertex=T.vertex
			and type hedge=T.hedge
			and module SetV=T.SetV
			and module SetH=T.SetH
			and module HashV=T.HashV
			and module HashH=T.HashH)

(*  ********************************************************************** *)
(** {2 Compare interface} *)
(*  ********************************************************************** *)

module Compare : sig
  val attrvertex :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> 'd
  val attrhedge :
    ('a, 'b) compare -> ('c, 'b, 'd, 'e, 'f) graph -> 'b -> 'e
  val is_vertex :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> bool
  val is_hedge :
    ('a, 'b) compare -> ('c, 'b, 'd, 'e, 'f) graph -> 'b -> bool
  val succhedge :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> 'c Sette.t
  val predhedge :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> 'c Sette.t
  val succvertex :
    ('a, 'b) compare -> ('c, 'b, 'd, 'e, 'f) graph -> 'b -> 'c array
  val predvertex :
    ('a, 'b) compare -> ('c, 'b, 'd, 'e, 'f) graph -> 'b -> 'c array
  val succ_vertex :
    ('a, 'b) compare -> ('a, 'b, 'c, 'd, 'e) graph -> 'a -> 'a Sette.t
  val pred_vertex :
    ('a, 'b) compare -> ('a, 'b, 'c, 'd, 'e) graph -> 'a -> 'a Sette.t
  val add_vertex :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> 'd -> unit
  val add_hedge :
    ('a, 'b) compare ->
    ('a, 'b, 'c, 'd, 'e) graph ->
    'b -> 'd -> pred:'a array -> succ:'a array -> unit
  val replace_attrvertex :
    ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a -> 'd -> unit
  val replace_attrhedge :
    ('a, 'b) compare -> ('a, 'b, 'c, 'd, 'e) graph -> 'b -> 'd -> unit
  val remove_hedge :
    ('a, 'b) compare -> ('a, 'b, 'c, 'd, 'e) graph -> 'b -> unit
  val remove_vertex :
    ('a, 'b) compare -> ('a, 'b, 'c, 'd, 'e) graph -> 'a -> unit
  val topological_sort :
    ('a, 'b) compare ->
    ?priority:'b priority -> ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'a list
  val topological_sort_multi :
    ('a, 'b) compare ->
    'a ->
    'b ->
    ?priority:'b priority ->
    ('a, 'b, 'c, 'd, 'e) t -> 'a Sette.t -> 'a list
  val reachable :
    ('a, 'b) compare ->
    ?filter:('b -> bool) ->
    ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'a Sette.t * 'b Sette.t
  val reachable_multi :
    ('a, 'b) compare ->
    'a ->
    'b ->
    ?filter:('b -> bool) ->
    ('a, 'b, 'c, 'd, 'e) t -> 'a Sette.t -> 'a Sette.t * 'b Sette.t
  val cfc :
    ('a, 'b) compare ->
    ?priority:'b priority ->
    ('a, 'b, 'c, 'd, 'e) graph -> 'a -> 'a list list
  val cfc_multi :
    ('a, 'b) compare ->
    ?priority:'b priority ->
    'a -> 'b -> ('a, 'b, 'c, 'd, 'e) graph -> 'a Sette.t -> 'a list list
  val scfc :
    ('a, 'b) compare ->
    ?priority:'b priority -> ('a, 'b, 'c, 'd, 'e) graph -> 'a -> (unit,'a) Ilist.t
  val scfc_multi :
    ('a, 'b) compare ->
    'a ->
    'b ->
    ?priority:'b priority ->
    ('a, 'b, 'c, 'd, 'e) graph -> 'a Sette.t -> (unit,'a) Ilist.t
  val print :
    ('a, 'b) compare ->
    (Format.formatter -> 'a -> unit) ->
    (Format.formatter -> 'b -> unit) ->
    (Format.formatter -> 'c -> unit) ->
    (Format.formatter -> 'd -> unit) ->
    (Format.formatter -> 'e -> unit) ->
    Format.formatter -> ('a, 'b, 'c, 'd, 'e) graph -> unit
  val min : ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a Sette.t
  val max : ('a, 'b) compare -> ('a, 'c, 'd, 'e, 'f) graph -> 'a Sette.t
end
