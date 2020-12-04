(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system: types *)

(** {2 Introduction}

    This module provides a generic engine for computing
    iteratively the solution of a fixpoint equation on a lattice.

    The equation system is represented with an hypergraph, in
    which vertices corresponds to unknown and oriented hyperedges
    to functions. It is assumed that hyperedges have unique
    destination vertex, and that associated functions are strict
    in each of their arguments: a bottom value in one of the
    argument implies that the result is empty.

    The equation system is parameterized by a manager, which
    provides the operations of the abstract lattices, and the
    transfer functions involved in the equation system.

    This interface is polymorphic and makes use of the following
    type variables:

    - ['vertex] is the type of vertex identifiers in the hypergraphs
    - ['hedge] is the type of hyperedges identifiers in the hypergraphs
    - ['abstract] is the type of abstract values (or dataflow properties)
    - ['arc] is the type of the information associated to hyperedges (optional features).

*)

open Format

(*  ********************************************************************** *)
(** {2 Public datatypes} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Manager} *)
(*  ====================================================================== *)

type ('vertex,'hedge,'abstract,'arc) manager = {
  (** Lattice operation *)
  mutable bottom : 'vertex -> 'abstract;
  mutable canonical : 'vertex -> 'abstract -> unit;
  mutable is_bottom : 'vertex -> 'abstract -> bool;
  mutable is_leq : 'vertex -> 'abstract -> 'abstract -> bool;
  mutable join :  'vertex -> 'abstract -> 'abstract -> 'abstract;
  mutable join_list : 'vertex -> 'abstract list -> 'abstract;
  mutable widening : 'vertex -> 'abstract -> 'abstract -> 'abstract;
  mutable odiff :  ('vertex -> 'abstract -> 'abstract -> 'abstract) option;

  (** Initialisation of equations *)
  mutable abstract_init : 'vertex -> 'abstract;
  mutable arc_init : 'hedge -> 'arc;

  (** Interpreting hyperedges *)
  mutable apply : 'hedge -> 'abstract array -> 'arc * 'abstract;

  (** Printing functions *)
  mutable print_vertex : Format.formatter -> 'vertex -> unit;
  mutable print_hedge : Format.formatter -> 'hedge -> unit;
  mutable print_abstract: Format.formatter -> 'abstract -> unit;
  mutable print_arc: Format.formatter -> 'arc -> unit;

  (** Fixpoint Options *)
  mutable accumulate : bool;

  (** Printing Options *)
  mutable print_fmt : Format.formatter;
  mutable print_analysis : bool;
  mutable print_component : bool;
  mutable print_step : bool;
  mutable print_state : bool;
  mutable print_postpre : bool;
  mutable print_workingsets : bool;

  (** DOT Options *)
  mutable dot_fmt : Format.formatter option;
  mutable dot_vertex : Format.formatter -> 'vertex -> unit;
  mutable dot_hedge : Format.formatter -> 'hedge -> unit;
  mutable dot_attrvertex : Format.formatter -> 'vertex -> unit;
  mutable dot_attrhedge : Format.formatter -> 'hedge -> unit;
}

(*  ====================================================================== *)
(** {3 Dynamically explored equation system} *)
(*  ====================================================================== *)

type ('vertex,'hedge) equation =
  'vertex -> ('hedge, 'vertex array * 'vertex) PMappe.t

(*  ====================================================================== *)
(** {3 Iteration strategies} *)
(*  ====================================================================== *)

type strategy_iteration = {
  mutable widening_start : int;
  mutable widening_descend : int;
  mutable ascending_nb : int;
  mutable descending_nb : int;
  mutable descending_stable : bool;
}

type ('vertex,'hedge) strategy_vertex = {
  mutable vertex : 'vertex;
  mutable hedges : 'hedge list;
  mutable widen : bool;
}
type ('vertex,'hedge) strategy =
    (strategy_iteration, ('vertex,'hedge) strategy_vertex) Ilist.t

let print_strategy_iteration fmt it =
  fprintf fmt "(%i,%i,%i,%i,%b)"
    it.widening_start it.widening_descend
    it.ascending_nb it.descending_nb it.descending_stable

let print_strategy_vertex
  (man:('vertex,'hedge,'abstract,'arc) manager)
  fmt
  (v:('vertex,'hedge) strategy_vertex)
  =
  fprintf fmt "@[<h>(%b,%a,%a)@]"
    v.widen
    man.print_vertex v.vertex
    (Print.list ~first:"@[<h>[" ~sep:"," ~last:"]@]"
	man.print_hedge)
    v.hedges

let print_strategy
  (man:('vertex,'hedge,'abstract,'arc) manager)
  fmt
  (s:('vertex,'hedge) strategy)
  =
  Ilist.print
    ~first:"[ @[<hv>"
    ~last:"@] ]"
    print_strategy_iteration
    (print_strategy_vertex man) fmt s

let make_strategy_iteration
    ?(widening_start=0)
    ?(widening_descend=1)
    ()
    = {
      widening_start = widening_start;
      widening_descend = widening_descend;
      ascending_nb = 0;
      descending_nb = 0;
      descending_stable = false;
    }

let make_strategy_vertex
    ?priority
    (graph:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (widen:bool)
    (vertex:'vertex)
    :
    ('vertex,'hedge) strategy_vertex
    =
  let spredhedges = PSHGraph.predhedge graph vertex in
  let hedges =
    PSette.fold
      (begin fun hedge res ->
	let takeit = match priority with
	  | None -> true
	  | Some(PSHGraph.Filter filter) -> filter hedge
	  | Some(PSHGraph.Priority p) -> (p hedge)>=0
	in
	if takeit then hedge::res else res
      end)
      spredhedges []
  in
  let strategy_vertex = {
    vertex = vertex;
    hedges = hedges;
    widen = widen
  }
  in
  strategy_vertex

let make_strategy_default
    ?(depth=2)
    ?(widening_start=0)
    ?(widening_descend=1)
    ?priority
    ~(vertex_dummy:'vertex)
    ~(hedge_dummy:'hedge)
    (graph:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (sinit:'vertex PSette.t)
    :
    ('vertex,'hedge) strategy
    =
  let scfc =
    PSHGraph.scfc_multi
      vertex_dummy hedge_dummy
      ?priority graph sinit
  in
  let scfc =
    Ilist.map
      (make_strategy_iteration ~widening_start ~widening_descend)
      (begin fun flag _ vertex ->
	make_strategy_vertex ?priority graph flag vertex
      end)
      scfc
  in
  Ilist.flatten ~depth scfc

(*  ====================================================================== *)
(** {3 Output} *)
(*  ====================================================================== *)

(** statistics at the end of the analysis *)
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

let ilist_map_condense f (it,ll) =
  let rec parcours res = function
    | [] -> res
    | Ilist.Atome(_)::ll -> parcours res ll
    | Ilist.List(it2,ll2)::ll ->
	let nit2 = f it2 in
	let nll2 = parcours [] ll2 in
	parcours ((Ilist.List(nit2, nll2))::res) ll
  in
  Ilist.rev ((f it),(parcours [] ll))

let stat_iteration_merge (it,ll) =
  let res = { nb = 0; stable = true } in
  let rec parcours = function
    | [] -> ()
    | (Ilist.Atome _)::ll -> parcours ll
    | (Ilist.List(it,ll2)::ll) ->
	res.nb <- max res.nb it.nb;
	res.stable <- res.stable && it.stable;
	parcours ll2; parcours ll
  in
  parcours ll;
  res

let print_stat_iteration fmt it =
  fprintf fmt "(%i,%b)" it.nb it.stable
let print_stat_iteration_ilist fmt ilist =
  Ilist.print
    print_stat_iteration (fun fmt _ -> ()) fmt ilist

let print_stat fmt stat =
  fprintf fmt "{ @[<hov>time=%f;@ ascending=%a;@ descending=%a;@] }"
    stat.time
    print_stat_iteration_ilist stat.ascending
    print_stat_iteration_ilist stat.descending

let print_output
    (man:('vertex,'hedge,'abstract,'arc) manager)
    fmt
    (g:('vertex,'hedge,'abstract,'arc, stat) PSHGraph.t)
    =
  PSHGraph.print
    man.print_vertex
    man.print_hedge
    man.print_abstract
    man.print_arc
    print_stat
    fmt
    g

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

(*  ====================================================================== *)
(** {3 Printing functions} *)
(*  ====================================================================== *)

let print_attr
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (fmt:Format.formatter)
    (attr:'abstract attr)
  =
  fprintf fmt "{ @[<hov>reach=%a;@ empty=%b@] }"
    man.print_abstract attr.reach
    attr.empty

let print_arc
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (fmt:Format.formatter)
    (arc:'arc arc)
  =
  fprintf fmt "{ @[<hov>arc=%a;@ aempty=%b@] }"
    man.print_arc arc.arc
    arc.aempty

let print_info
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (fmt:Format.formatter)
    (info:('vertex,'hedge) info)
  =
  fprintf fmt "{ @[<hov>itime=%f;@ iascending=%a;@ idescending=%a;@ iworkvertex=%a;@ iworkhedge=%a@] }"
    !(info.itime)
    print_stat_iteration_ilist info.iascending
    print_stat_iteration_ilist info.idescending
    (PHashhe.print man.print_vertex (fun _ _ -> ())) info.iworkvertex
    (PHashhe.print man.print_hedge (fun _ _ -> ())) info.iworkhedge

let print_workingsets
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (fmt:Format.formatter)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    :
    unit
    =
  let hashprint print fmt x =
      PHashhe.print
	~first:"[@[<hov>" ~last:"@]]"
	~firstbind:"" ~sepbind:"" ~lastbind:""
	print (fun _ _ -> ())
	fmt x
  in
  let info = PSHGraph.info graph in
  fprintf fmt "@[<v>workvertex = %a@ workhedge = %a"
    (hashprint man.print_vertex) info.iworkvertex
    (hashprint man.print_hedge) info.iworkhedge
  ;
  begin match info.iinfodyn with
  | Some(dyn) ->
      fprintf fmt "@ addhedge = %a"
	(hashprint man.print_hedge) dyn.iaddhedge
  | None -> ()
  end;
  fprintf fmt "@]"

let print_graph
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (fmt:Format.formatter)
    (g:('vertex,'hedge,'abstract,'arc) graph)
    =
  PSHGraph.print
    man.print_vertex
    man.print_hedge
    (print_attr man)
    (print_arc man)
    (print_info man)
    fmt
    g

(*  ====================================================================== *)
(** {3 DOT functions} *)
(*  ====================================================================== *)

let set2_of_strategy
    comparev
    (strategy:('vertex,'hedge) strategy)
    :
    'vertex PSette.t * 'vertex PSette.t
    =
  let empty = PSette.empty comparev in
  Ilist.fold_left
    (begin fun (res,resw) _ _ v ->
      if v.widen then
	res,(PSette.add v.vertex resw)
      else
	(PSette.add v.vertex res),resw
    end)
    (empty,empty) strategy

let dot_graph
    ?(style="size=\"7.5,10\";center=true;ranksep=0.16;nodesep=0.02;")
    ?(titlestyle="fontsize=18")
    ?(vertexstyle="shape=box,fontsize=18,height=0.02,width=0.01")
    ?(hedgestyle="shape=plaintext,fontsize=18,height=0.0,width=0.01")
    ?strategy
    ?vertex
    (man:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    ~(title:string)
    =
  let comparev = graph.PSHGraph.compare.PSHGraph.comparev in
  match man.dot_fmt with
  | None -> ()
  | Some dot_fmt ->
      let (set,setw) = match strategy with
	| None -> let e = PSette.empty comparev in (e,e)
	| Some strategy -> set2_of_strategy comparev strategy
      in
      let info = PSHGraph.info graph in
      let fvertexstyle = begin fun v ->
	let vertexstyle =
	  if begin match vertex with
	  | None -> false
	  | Some vertex -> (graph.PSHGraph.compare.PSHGraph.comparev v vertex) = 0
	  end
	  then
	    vertexstyle^",style=filled,fillcolor=coral1"
	  else if PSette.mem v setw then
	    vertexstyle^",style=filled,fillcolor=red1"
	  else if PSette.mem v set then
	    vertexstyle^",style=filled,fillcolor=orange1"
	  else
	    vertexstyle
	in
	let vertexstyle =
	  if PHashhe.mem info.iworkvertex v then
	    vertexstyle^",fontcolor=blue3"
	  else
	    vertexstyle
	in
	vertexstyle
      end
      in
      let fhedgestyle = begin fun h ->
	let hedgestyle =
	  if PHashhe.mem info.iworkhedge h then
	    hedgestyle^",fontcolor=blue3"
	  else
	    hedgestyle
	in
	hedgestyle
      end
      in
      PSHGraph.print_dot
	~titlestyle
	~style
	~fvertexstyle
	~fhedgestyle
	~title
	(fun fmt vertex ->
	  fprintf fmt "\"%s\""
	    (Print.escaped ~linebreak:'n'
	      (Print.sprintf "%a" man.dot_vertex vertex)))
	(fun fmt hedge ->
	  fprintf fmt "\"%s\""
	    (Print.escaped ~linebreak:'n'
	      (Print.sprintf "%a" man.dot_hedge hedge)))
	(fun fmt vertex attr ->
	  fprintf fmt "%s"
	    (Print.escaped ~linebreak:'n'
	      (Print.sprintf ~margin:40 "%a@.%a"
		man.dot_attrvertex vertex
		man.print_abstract attr.reach)))
	(fun fmt hedge arc ->
	  fprintf fmt "%s"
	    (Print.escaped ~linebreak:'n'
	      (Print.sprintf ~margin:40 "%a@.%a"
		man.dot_attrhedge hedge
		man.print_arc arc.arc)))
	dot_fmt
	graph
