(** Oriented hypergraphs, parametrized polymorphic version. *)

(** Same interface as {!SHGraph}, but each graph stores its comparison
    functions.

    See the comment about comparison functions in the module
    {!PSette}. *)

type 'a priority = 'a SHGraph.priority =
  | Filter of ('a -> bool)
  | Priority of ('a -> int)

type ('a,'b) compare = ('a,'b) SHGraph.compare = {
  hashv : 'a Hashhe.compare;
  hashh : 'b Hashhe.compare;
  comparev : 'a -> 'a -> int;
  compareh : 'b -> 'b -> int;
}

type ('a, 'b, 'c, 'd, 'e) t = {
  compare : ('a, 'b) compare;
  mutable graph : ('a, 'b, 'c, 'd, 'e) SHGraph.t;
}

val stdcompare : ('a,'b) compare

val make :
  ('a, 'b) compare ->
  ('a, 'b, 'c, 'd, 'e) SHGraph.t -> ('a, 'b, 'c, 'd, 'e) t
val create_compare :
  ('a, 'b) compare -> int -> 'c -> ('a, 'b, 'd, 'e, 'c) t
val create : ('a, 'b) compare -> int -> 'c -> ('a, 'b, 'd, 'e, 'c) t
val clear : ('a, 'b, 'c, 'd, 'e) t -> unit
val size_vertex : ('a, 'b, 'c, 'd, 'e) t -> int
val size_hedge : ('a, 'b, 'c, 'd, 'e) t -> int
val size_edgevh : ('a, 'b, 'c, 'd, 'e) t -> int
val size_edgehv : ('a, 'b, 'c, 'd, 'e) t -> int
val size : ('a, 'b, 'c, 'd, 'e) t -> int * int * int * int
val attrvertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'c
val attrhedge : ('a, 'b, 'c, 'd, 'e) t -> 'b -> 'd
val info : ('a, 'b, 'c, 'd, 'e) t -> 'e
val is_vertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> bool
val is_hedge : ('a, 'b, 'c, 'd, 'e) t -> 'b -> bool
val is_empty : ('a, 'b, 'c, 'd, 'e) t -> bool
val succhedge : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'b PSette.t
val predhedge : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'b PSette.t
val succvertex : ('a, 'b, 'c, 'd, 'e) t -> 'b -> 'a array
val predvertex : ('a, 'b, 'c, 'd, 'e) t -> 'b -> 'a array
val succ_vertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'a PSette.t
val pred_vertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'a PSette.t
val add_vertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'c -> unit
val add_hedge :
  ('a, 'b, 'c, 'd, 'e) t ->
  'b -> 'd -> pred:'a array -> succ:'a array -> unit
val replace_attrvertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> 'c -> unit
val replace_attrhedge : ('a, 'b, 'c, 'd, 'e) t -> 'b -> 'd -> unit
val remove_hedge : ('a, 'b, 'c, 'd, 'e) t -> 'b -> unit
val remove_vertex : ('a, 'b, 'c, 'd, 'e) t -> 'a -> unit
val iter_vertex :
  ('a, 'b, 'c, 'd, 'e) t ->
  ('a -> 'c -> pred:'b PSette.t -> succ:'b PSette.t -> unit) -> unit
val fold_vertex :
  ('a, 'b, 'c, 'd, 'e) t ->
  ('a -> 'c -> pred:'b PSette.t -> succ:'b PSette.t -> 'f -> 'f) ->
  'f -> 'f
val iter_hedge :
  ('a, 'b, 'c, 'd, 'e) t ->
  ('b -> 'd -> pred:'a array -> succ:'a array -> unit) -> unit
val fold_hedge :
  ('a, 'b, 'c, 'd, 'e) t ->
  ('b -> 'd -> pred:'a array -> succ:'a array -> 'f -> 'f) -> 'f -> 'f
val map :
  ('a, 'b, 'c, 'd, 'e) t ->
  ('a -> 'c -> 'f) ->
  ('b -> 'd -> 'g) ->
  ('e -> 'h) -> ('a, 'b, 'f, 'g, 'h) t
val copy :
  ('a -> 'b -> 'c) ->
  ('d -> 'e -> 'f) ->
  ('g -> 'h) -> ('a, 'd, 'b, 'e, 'g) t -> ('a, 'd, 'c, 'f, 'h) t
val transpose :
  ('a -> 'b -> 'c) ->
  ('d -> 'e -> 'f) ->
  ('g -> 'h) -> ('a, 'd, 'b, 'e, 'g) t -> ('a, 'd, 'c, 'f, 'h) t
val topological_sort :
  ?priority:'a priority -> ('b, 'a, 'c, 'd, 'e) t -> 'b -> 'b list
val topological_sort_multi :
  'a ->
  'b ->
  ?priority:'b priority ->
  ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t -> 'a list
val reachable :
  ?filter:('a -> bool) ->
  ('b, 'a, 'c, 'd, 'e) t -> 'b -> 'b PSette.t * 'a PSette.t
val reachable_multi :
  'a -> 'b -> ?filter:('b -> bool) ->
  ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t -> 'a PSette.t * 'b PSette.t
val cfc :
  ?priority:'a priority -> ('b, 'a, 'c, 'd, 'e) t -> 'b -> 'b list list
val cfc_multi :
  'a -> 'b -> ?priority:'b priority ->
  ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t -> 'a list list
val scfc :
  ?priority:'a priority -> ('b, 'a, 'c, 'd, 'e) t -> 'b -> (unit,'b) Ilist.t
val scfc_multi :
  'a -> 'b ->	?priority:'b priority ->
  ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t -> (unit,'a) Ilist.t
val print :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  (Format.formatter -> 'd -> unit) ->
  (Format.formatter -> 'e -> unit) ->
  Format.formatter -> ('a, 'b, 'c, 'd, 'e) t -> unit
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
  Format.formatter -> ('a, 'b, 'c, 'd, 'e) t -> unit
val min : ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t
val max : ('a, 'b, 'c, 'd, 'e) t -> 'a PSette.t
