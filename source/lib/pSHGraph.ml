(** Oriented hypergraphs, parametrized polymorphic version. *)

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

let stdcompare = SHGraph.stdcompare

open SHGraph

let make compare graph = { compare=compare; graph=graph }
let create_compare compare n info = make compare (SHGraph.create n info)
let create = create_compare
let clear t = SHGraph.clear t.graph

let size_vertex t = SHGraph.size_vertex t.graph
let size_hedge t = SHGraph.size_hedge t.graph
let size_edgevh t = SHGraph.size_edgevh t.graph
let size_edgehv t = SHGraph.size_edgehv t.graph
let size t = SHGraph.size t.graph

let attrvertex t v = SHGraph.Compare.attrvertex t.compare t.graph v
let attrhedge t h = SHGraph.Compare.attrhedge t.compare t.graph h
let info t = SHGraph.info t.graph

let is_vertex t v = SHGraph.Compare.is_vertex t.compare t.graph v
let is_hedge t h =  SHGraph.Compare.is_hedge t.compare t.graph h
let is_empty t = SHGraph.is_empty t.graph

let succhedge t v = PSette.make t.compare.compareh (SHGraph.Compare.succhedge t.compare t.graph v)
let predhedge t v = PSette.make t.compare.compareh (SHGraph.Compare.predhedge t.compare t.graph v)
let succvertex t h = SHGraph.Compare.succvertex t.compare t.graph h
let predvertex t h = SHGraph.Compare.predvertex t.compare t.graph h
let succ_vertex t v = PSette.make t.compare.comparev (SHGraph.Compare.succ_vertex t.compare t.graph v)
let pred_vertex t v = PSette.make t.compare.comparev (SHGraph.Compare.pred_vertex t.compare t.graph v)

let add_vertex t v attrvertex =
  SHGraph.Compare.add_vertex t.compare t.graph v attrvertex
let add_hedge t h attrhedge ~pred ~succ =
  SHGraph.Compare.add_hedge t.compare t.graph h attrhedge ~pred ~succ

let replace_attrvertex t v attrvertex =
  SHGraph.Compare.replace_attrvertex t.compare t.graph v attrvertex
let replace_attrhedge t h attrhedge =
  SHGraph.Compare.replace_attrhedge t.compare t.graph h attrhedge

let remove_hedge t h = SHGraph.Compare.remove_hedge t.compare t.graph h
let remove_vertex t v = SHGraph.Compare.remove_vertex t.compare t.graph v

let iter_vertex t f =
  Hashhe.iter
    (fun v vertex_n -> f v vertex_n.attrvertex ~pred:(PSette.make t.compare.compareh vertex_n.predhedge) ~succ:(PSette.make t.compare.compareh vertex_n.succhedge))
    t.graph.vertex
let iter_hedge t f =
  Hashhe.iter
    (fun n hedge_n -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex)
    t.graph.hedge

let fold_vertex t f res =
  Hashhe.fold
    (fun v vertex_n res -> f v vertex_n.attrvertex ~pred:(PSette.make t.compare.compareh vertex_n.predhedge) ~succ:(PSette.make t.compare.compareh vertex_n.succhedge) res)
    t.graph.vertex res
let fold_hedge t f res =
  Hashhe.fold
    (fun n hedge_n res -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex res)
    t.graph.hedge res

let map t map_attrvertex map_attrhedge map_info = make t.compare (map t.graph map_attrvertex map_attrhedge map_info)
let copy copy_attrvertex copy_attrhedge copy_info t = make t.compare (copy copy_attrvertex copy_attrhedge copy_info t.graph)
let transpose copy_attrvertex copy_attrhedge copy_info t = make t.compare (transpose copy_attrvertex copy_attrhedge copy_info t.graph)

let topological_sort ?priority t root =
  SHGraph.Compare.topological_sort t.compare ?priority t.graph root

let check_pset t root =
    if t.compare.comparev==root.PSette.compare then true
    else begin
      Format.eprintf "@.PSHGraph: the comparison function for sets of vertices in the graph (%i) is not physically the same as the comparison function for elements in the set (%i).@.See the documentation of the module PSette.@." (Obj.magic t.compare.comparev) (Obj.magic root.PSette.compare);
      false
    end

let topological_sort_multi vertex_dummy hedge_dummy ?priority t root =
  assert(check_pset t root);
  SHGraph.Compare.topological_sort_multi t.compare vertex_dummy hedge_dummy ?priority t.graph root.PSette.set

let reachable ?filter t root =
  let (setv,seth) =
    SHGraph.Compare.reachable t.compare ?filter t.graph root
  in
  (PSette.make t.compare.comparev setv,
  PSette.make t.compare.compareh seth)

let reachable_multi vertex_dummy hedge_dummy ?filter t root =
  assert(check_pset t root);
  let (setv,seth) =
    SHGraph.Compare.reachable_multi t.compare vertex_dummy hedge_dummy ?filter t.graph root.PSette.set
  in
  (PSette.make t.compare.comparev setv,
  PSette.make t.compare.compareh seth)

let cfc ?priority t root =
  SHGraph.Compare.cfc t.compare ?priority t.graph root

let cfc_multi vertex_dummy hedge_dummy ?priority t root =
  assert(check_pset t root);
  SHGraph.Compare.cfc_multi t.compare vertex_dummy hedge_dummy ?priority t.graph root.PSette.set

let scfc ?priority t root  =
  SHGraph.Compare.scfc t.compare ?priority t.graph root

let scfc_multi vertex_dummy hedge_dummy ?priority t root  =
  assert(check_pset t root);
  SHGraph.Compare.scfc_multi t.compare vertex_dummy hedge_dummy ?priority t.graph root.PSette.set

let print pv ph pattrv pattrh pinfo fmt t =
  SHGraph.Compare.print t.compare pv ph pattrv pattrh pinfo fmt t.graph

let print_dot
    ?style
    ?titlestyle
    ?vertexstyle
    ?hedgestyle
    ?fvertexstyle
    ?fhedgestyle
    ?title
    print_vertex print_hedge print_attrvertex print_attrhedge
    fmt t
    =
  SHGraph.print_dot ?titlestyle
    ?vertexstyle ?hedgestyle
    ?fvertexstyle ?fhedgestyle
    ?title
    print_vertex print_hedge print_attrvertex print_attrhedge
    fmt t.graph

let min t = PSette.make t.compare.comparev (SHGraph.Compare.min t.compare t.graph)
let max t = PSette.make t.compare.comparev (SHGraph.Compare.max t.compare t.graph)
