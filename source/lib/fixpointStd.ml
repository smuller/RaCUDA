(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Fixpoint analysis of an equation system *)

open Format
open FixpointType

(*  ********************************************************************** *)
(** {2 Utilities} *)
(*  ********************************************************************** *)

(** Does the array of vertices belong to the graph, all with non
    bottom values ? *)
let is_tvertex
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (tvertex:'vertex array)
    :
    bool
    =
  try
    Array.iter
      (begin fun vertex ->
	let attr = PSHGraph.attrvertex graph vertex in
	if attr.empty then raise Exit;
      end)
      tvertex
    ;
    true
  with Exit | Not_found ->
    false

(** Return the array of abstract values associated to the vertices
*)
let treach_of_tvertex
    ~(descend:bool)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (tvertex:'vertex array)
    :
    'abstract array
    =
  if not descend && (Array.length tvertex)=1 then
    let attr = PSHGraph.attrvertex graph tvertex.(0) in
    [| attr.diff |]
  else
    Array.map
      (begin fun vertex ->
	let attr = PSHGraph.attrvertex graph vertex in
	attr.reach
      end)
      tvertex

(** Update working sets assuming that the abstract value
    associated to the vertex has been modified. If [hedge=true], then
    also consider the working set associated to hyperhedges. *)
let update_workingsets
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    ~(hedge:bool)
    (vertex:'vertex)
    :
    unit
    =
  let info = PSHGraph.info graph in
  begin match info.iinfodyn with
  | None ->
      let seth = PSHGraph.succhedge graph vertex in
      PSette.iter
	(begin fun (h:'hedge) ->
	  if hedge then PHashhe.replace info.iworkhedge h ();
	  let succ = PSHGraph.succvertex graph h in
	  assert ((Array.length succ)=1);
	  PHashhe.replace info.iworkvertex succ.(0) ();
	end)
	seth
      ;
  | Some(dyn) ->
      let maph = dyn.iequation vertex in
      PMappe.iter
	(begin fun h ((tpredvertex,succvertex) as tpredsucc) ->
	  if (PSHGraph.is_hedge graph h) then begin
	    if hedge then PHashhe.replace info.iworkhedge h ();
	    PHashhe.replace info.iworkvertex succvertex ();
	  end
	  else begin
	    PHashhe.replace dyn.iaddhedge h tpredsucc;
	  end
	end)
	maph
  end

(*  ********************************************************************** *)
(** {2 Initialisation of fixpoint computation} *)
(*  ********************************************************************** *)

(** [init manager input sinit] creates the internal graph
    structure (from the equation graph [input]) and initialize the
    working sets (from the set of initial points [sinit]) (stored
    in the info field of the internal graph). *)
let init
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (input:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (sinit:'vertex PSette.t)
    :
    ('vertex,'hedge,'abstract,'arc) graph
    =
  let make_attr vertex _ : 'abstract attr
      =
    let (empty,def) =
      if PSette.mem vertex sinit
      then (false,manager.abstract_init vertex)
      else (true,manager.bottom vertex)
    in {
      reach = def;
      diff = def;
      empty = empty;
    }
  in
  let make_arc hedge _ : 'arc arc
      =
    { arc = manager.arc_init hedge; aempty = true }
  in
  let make_info _ : ('vertex,'hedge) info
      =
    {
      iinit = sinit;
      itime = ref 0.0;
      iascending = ({nb=0; stable=false}, []);
      idescending = ({nb=0; stable=false}, []);
      iworkvertex = PHashhe.create_compare input.PSHGraph.compare.PSHGraph.hashv 7;
      iworkhedge = PHashhe.create_compare input.PSHGraph.compare.PSHGraph.hashh 7;
      iinfodyn = None;
    }
  in
  let graph =
    PSHGraph.copy make_attr make_arc make_info input
  in
  PSette.iter
    (update_workingsets graph ~hedge:manager.accumulate)
    sinit
  ;
  graph

(*  ********************************************************************** *)
(** {2 Process a vertex} *)
(*  ********************************************************************** *)

(*  ====================================================================== *)
(** {3 Accumulate} *)
(*  ====================================================================== *)

(** Compute the least upper bound of the current value of the
    vertex/variable with the values propagated by the incoming
    hyperedges belonging to the working set. Returns [true] if the
    value is strictly increasing. *)

let av_print_intro manager vertex =
  fprintf manager.print_fmt "processing (acc) vertex %a:@   @[<v>"
    manager.print_vertex vertex

let av_print_oldreach manager oldreach =
  fprintf manager.print_fmt "contrib of oldreach = %a@ "
    manager.print_abstract oldreach
let av_print_contrib manager hedge post =
  fprintf manager.print_fmt "contrib of hedge %a = %a@ "
    manager.print_hedge hedge manager.print_abstract post

let av_print_result manager graph vertex attr growing =
  fprintf manager.print_fmt "reach=%a" manager.print_abstract attr.reach;
  if manager.accumulate && manager.odiff<>None then
    fprintf manager.print_fmt "@ diff=%a" manager.print_abstract attr.diff;
  if not growing then fprintf manager.print_fmt "@ no change";
  fprintf manager.print_fmt "@]@ ";
  if manager.dot_fmt<>None then
    dot_graph manager graph
      ~vertex
      ~title:(
	Print.sprintf "processed (acc) %a"
	  manager.dot_vertex vertex
      )
  ;
  ()

let accumulate_vertex
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (svertex:('vertex,'hedge) strategy_vertex)
    (attr:'abstract attr)
    :
    bool
    =
  let info = PSHGraph.info graph in
  let vertex = svertex.vertex in
  let lhedges = svertex.hedges in
  let oldreach = attr.reach in
  if manager.print_state then av_print_intro manager vertex;

  PHashhe.remove info.iworkvertex vertex;
  let lpost = ref [] in
  let allpost = ref true in
  if manager.print_postpre then av_print_oldreach manager oldreach;
  List.iter
    (begin fun hedge ->
      if not manager.accumulate || PHashhe.mem info.iworkhedge hedge then begin
	PHashhe.remove info.iworkhedge hedge;
	let tpredvertex = PSHGraph.predvertex graph hedge in
	if is_tvertex graph tpredvertex then begin
	  let attrhedge = PSHGraph.attrhedge graph hedge in
	  let treach = treach_of_tvertex ~descend:false graph tpredvertex in
	  let (arc,post) = manager.apply hedge treach in
	  attrhedge.arc <- arc;
	  if manager.print_postpre then av_print_contrib manager hedge post;
	  if not (attrhedge.aempty && manager.is_bottom vertex post) then begin
	    lpost := post :: !lpost;
	    attrhedge.aempty <- false;
	    attr.empty <- false;
	  end
	  else allpost := false;
	end
	else allpost := false;
      end
      else allpost := false;
    end)
    lhedges
  ;
  let lpost2 =
    if not !allpost || svertex.widen || PSette.mem vertex info.iinit then
      oldreach :: !lpost
    else
      !lpost
  in
  attr.reach <- manager.join_list vertex lpost2;
  manager.canonical vertex attr.reach;
  let growing =
    match manager.odiff with
    | Some(diff) when manager.accumulate ->
	let nabs = match !lpost with
	  | [post] -> post
	  | _ -> attr.reach
	in
	attr.diff <- diff vertex nabs oldreach;
	not (manager.is_bottom vertex attr.diff)
    | _ ->
	attr.diff <- attr.reach;
	not (manager.is_leq vertex attr.reach oldreach)
  in
  if manager.print_state then av_print_result manager graph vertex attr growing;
  growing

(*  ====================================================================== *)
(** {3 Propagate} *)
(*  ====================================================================== *)

(** Compute the least upper bound of the values propagated by all
    the incoming hyperedges. Returns [true] if the value is
    strictly increasing. If [descend=true], the value is supposed
    to be decreasing and returns true if it is strictly
    decreasing. *)

let pv_print_intro manager vertex =
  fprintf manager.print_fmt "processing (prop) vertex %a:@   @[<v>"
    manager.print_vertex vertex
let pv_print_result manager graph vertex attr update =
  fprintf manager.print_fmt "reach=%a" manager.print_abstract attr.reach;
  if not update then fprintf manager.print_fmt "@ no change";
  fprintf manager.print_fmt "@]@ ";
  if manager.dot_fmt<>None then
    dot_graph manager graph
      ~vertex
      ~title:(
	Print.sprintf "processed (prop) %a"
	  manager.dot_vertex vertex
      )
  ;
  ()

let propagate_vertex
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    ~(descend:bool)
    (svertex:('vertex,'hedge) strategy_vertex)
    (attr:'abstract attr)
    :
    bool
    =
  let info = PSHGraph.info graph in
  let vertex = svertex.vertex in
  let lhedges = svertex.hedges in
  let oldreach = attr.reach in
  if manager.print_state then pv_print_intro manager vertex;

  PHashhe.remove info.iworkvertex vertex;

  let (lpost:'abstract list ref) = ref [] in
  if PSette.mem vertex info.iinit then
      lpost := (manager.abstract_init vertex) :: !lpost
  ;
  List.iter
    (begin fun hedge ->
      let tpredvertex = PSHGraph.predvertex graph hedge in
      let attrhedge = PSHGraph.attrhedge graph hedge in
      let takeit =
	(if descend then not attrhedge.aempty else true)
	&& is_tvertex graph tpredvertex
      in
      if takeit then begin
	PHashhe.remove info.iworkhedge hedge;
	let treach = treach_of_tvertex ~descend graph tpredvertex in
	let (arc,post) = manager.apply hedge treach in
	attrhedge.arc <- arc;
	if manager.print_postpre then av_print_contrib manager hedge post;
	if not (manager.is_bottom vertex post) then begin
	  lpost := post :: !lpost;
	  attrhedge.aempty <- false
	end
	else
	  attrhedge.aempty <- true;
      end
      else
	attrhedge.aempty <- true;
    end)
    lhedges
  ;
  attr.reach <-
    if !lpost=[]
    then manager.bottom vertex
    else  manager.join_list vertex !lpost;
  manager.canonical vertex attr.reach;
  attr.diff <- attr.reach;
  attr.empty <- manager.is_bottom vertex attr.reach;
  let update =
    if descend
    then not (manager.is_leq vertex oldreach attr.reach)
    else not (manager.is_leq vertex attr.reach oldreach)
  in
  if manager.print_state then pv_print_result manager graph vertex attr update;
  update

(*  ====================================================================== *)
(** {3 Accumulate and update} *)
(*  ====================================================================== *)

let p_print_result manager graph vertex attr =
  fprintf manager.print_fmt "  widening = %a@ "
    manager.print_abstract attr.reach;
  if manager.accumulate && manager.odiff<>None then
    fprintf manager.print_fmt "  diff=%a@ " manager.print_abstract attr.diff;
  if manager.dot_fmt<>None then
    dot_graph manager graph ~vertex ~title:"after widening"

let process_vertex
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    ~(widening:bool)
    (svertex:('vertex,'hedge) strategy_vertex)
    :
    bool
    =
  let vertex = svertex.vertex in
  let attr = PSHGraph.attrvertex graph vertex in
  let oldreach = attr.reach in
  let oldempty = attr.empty in
  let growing =
    (if manager.accumulate || svertex.widen && widening then
      accumulate_vertex
    else
      propagate_vertex ~descend:false
    )
      manager graph svertex attr
  in
  if growing then begin
    update_workingsets ~hedge:manager.accumulate graph vertex;
  end;
  if manager.print_workingsets then fprintf manager.print_fmt "  %a@ " (print_workingsets manager) graph;
  if growing && svertex.widen then begin
    if widening && not oldempty then begin
      attr.reach <- manager.widening vertex oldreach attr.reach;
      manager.canonical vertex attr.reach;
      attr.diff <- begin match manager.odiff with
      | Some(diff) when manager.accumulate ->
	  diff vertex attr.reach oldreach;
      | _ ->
	  attr.reach;
      end;
      if manager.print_state then p_print_result manager graph vertex attr;
    end;
    assert (not (manager.is_leq vertex attr.reach oldreach));
  end;
  growing

(*  ********************************************************************** *)
(** {2 Process a descending strategy of depth 2 (a strongly
    connected component} *)
(*  ********************************************************************** *)

let d_print_intro manager graph strategy =
  fprintf manager.print_fmt "Descending (linearized) strategy@   %a@   @[<v>"
    (print_strategy manager) strategy
  ;
  if manager.dot_fmt<>None then begin
    dot_graph manager graph ~strategy ~title:"Descending (linearized) strategy"
  end

let d_print_step manager graph strategy counter =
  fprintf manager.print_fmt "Sum up of the descending step (%i iterations)@ "
    !counter;
  Ilist.iter
    (begin fun _ _ svertex ->
      let vertex = svertex.vertex in
      let attrvertex = PSHGraph.attrvertex graph vertex in
      fprintf manager.print_fmt "  acc(%a)=%a@ "
	manager.print_vertex vertex
	manager.print_abstract attrvertex.reach
    end)
    strategy
  ;
  if manager.dot_fmt<>None then
    dot_graph manager graph ~strategy ~title:(Print.sprintf "Sum up of the descending step (%i iterations)" !counter)

let d_print_result manager graph strategy =
  fprintf manager.print_fmt "End descending strategy";
  if true then begin
    Ilist.iter
      (begin fun _ _ svertex ->
	let vertex = svertex.vertex in
	let attrvertex = PSHGraph.attrvertex graph vertex in
	fprintf manager.print_fmt "@   acc(%a) =%a"
	  manager.print_vertex vertex
	  manager.print_abstract attrvertex.reach
      end)
      strategy
  end;
  fprintf manager.print_fmt "@]@ ";
  if manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~strategy
      ~title:"End Descending strategy"
  end

let descend_strategy
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    strategy
    :
    bool
    =
  let info = PSHGraph.info graph in
  let process_svertex svertex : bool =
    if PHashhe.mem info.iworkvertex svertex.vertex
    then begin
      let attr = PSHGraph.attrvertex graph svertex.vertex in
      let reducing =
	propagate_vertex ~descend:true manager graph svertex attr
      in
      if reducing then begin
	update_workingsets ~hedge:false graph svertex.vertex;
      end;
      if manager.print_workingsets then fprintf manager.print_fmt "  %a@ " (print_workingsets manager) graph;
      reducing
    end
    else
      false
  in

  if manager.print_component then d_print_intro manager graph strategy;
  let (it,_) = strategy in
  let reducing = ref true in
  let counter = ref 0 in
  while !reducing && !counter < it.widening_descend do
    reducing := false;
    incr counter;
    (* Linear iteration on vertices of a strongly connected component *)
    Ilist.iter
      (begin fun _ _ svertex ->
	let reducing2 = process_svertex svertex in
	reducing := !reducing || reducing2
      end)
      strategy
    ;
    reducing := !reducing && (PHashhe.length info.iworkvertex) > 0;
    if !reducing && manager.print_step then d_print_step manager graph strategy counter;
  done;
  if manager.print_component then d_print_result manager graph strategy;
  it.descending_nb <- !counter;
  it.descending_stable <- not !reducing;
  !reducing

(*  ********************************************************************** *)
(** {2 Descending sequence} *)
(*  ********************************************************************** *)

let descend
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (strategy:('vertex,'hedge) strategy)
    :
    bool
    =
  let (it,_) = strategy in
  if it.widening_descend>0 then begin
    let info = PSHGraph.info graph in
    let oldworkvertex = PHashhe.copy info.iworkvertex in
    let oldworkhedge = PHashhe.copy info.iworkhedge in
    PHashhe.clear info.iworkvertex;
    PHashhe.clear info.iworkhedge;
    Ilist.iter
      (begin fun _ _ svertex ->
	let vertex = svertex.vertex in
	let attrvertex = PSHGraph.attrvertex graph vertex in
	if not attrvertex.empty then
	  PHashhe.replace info.iworkvertex vertex ()
      end)
      strategy
    ;
    if manager.print_workingsets then fprintf manager.print_fmt "  %a@ " (print_workingsets manager) graph;
    ignore (descend_strategy manager graph strategy);
    let reducing = (PHashhe.length info.iworkvertex) > 0 in
    info.iworkvertex <- oldworkvertex;
    info.iworkhedge <- oldworkhedge;
    reducing
  end else begin
    it.descending_nb <- 0;
    it.descending_stable <- false;
    false;
  end

(*  ********************************************************************** *)
(** {2 Process a (recursive) strategy of depth 2 or more} *)
(*  ********************************************************************** *)

let s_print_intro ~depth manager graph strategy =
  fprintf manager.print_fmt "Processing strategy at depth=%i@   %a@   @[<v>"
    depth (print_strategy manager) strategy;
  if manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~strategy
      ~title:(Print.sprintf "Processing strategy at depth %i" depth)
  end

let tops_print_intro manager graph strategy =
  fprintf manager.print_fmt "Processing toplevel strategy@   %a@   @[<v>"
    (print_strategy manager) strategy;
  if manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~strategy
      ~title:"Processing toplevel strategy"
  end

let s_print_step manager graph strategy nsteps growing =
  fprintf manager.print_fmt "Sum up of the looping step (%i iterations) (growing=%b)@ " !nsteps !growing;
  Ilist.iter
    (begin fun _ _ strategy_vertex ->
      let vertex = strategy_vertex.vertex in
      let attrvertex = PSHGraph.attrvertex graph vertex in
      fprintf manager.print_fmt "  acc (%a)=%a@ "
	manager.print_vertex vertex
	manager.print_abstract attrvertex.reach
      ;
      if manager.accumulate && manager.odiff<>None then
	fprintf manager.print_fmt "@   diff(%a)=%a"
	  manager.print_vertex vertex
	  manager.print_abstract attrvertex.diff
    end)
    strategy
  ;
  if manager.print_workingsets then
    fprintf manager.print_fmt "  %a@ " (print_workingsets manager) graph;

  if manager.dot_fmt<>None then begin
    dot_graph manager graph ~strategy
      ~title:(Print.sprintf "Sum up of the looping step (%i iterations)" !nsteps)
  end

let s_print_result ~depth manager graph strategy =
  fprintf manager.print_fmt "End processing strategy at depth %i" depth;
  if true then begin
    Ilist.iter
      (begin fun _ _ svertex ->
	let vertex = svertex.vertex in
	let attrvertex = PSHGraph.attrvertex graph vertex in
	fprintf manager.print_fmt "@   acc (%a)=%a"
	  manager.print_vertex vertex
	  manager.print_abstract attrvertex.reach
	;
	if manager.accumulate && manager.odiff<>None then
	  fprintf manager.print_fmt "@   diff(%a)=%a"
	    manager.print_vertex vertex
	    manager.print_abstract attrvertex.diff
      end)
      strategy
  end;
  fprintf manager.print_fmt "@]@ ";
  if manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~strategy
      ~title:(Print.sprintf "End Processing strategy at depth %i" depth)
  end

let tops_print_result manager graph strategy =
  fprintf manager.print_fmt "End processing toplevel strategy";
  if true then begin
    Ilist.iter
      (begin fun _ _ svertex ->
	let vertex = svertex.vertex in
	let attrvertex = PSHGraph.attrvertex graph vertex in
	fprintf manager.print_fmt "@   acc (%a)=%a"
	  manager.print_vertex vertex
	  manager.print_abstract attrvertex.reach
	;
	if manager.accumulate && manager.odiff<>None then
	  fprintf manager.print_fmt "@   diff(%a)=%a"
	    manager.print_vertex vertex
	    manager.print_abstract attrvertex.diff
      end)
      strategy
  end;
  fprintf manager.print_fmt "@]@ ";
  if manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~strategy
      ~title:"End Processing toplevel strategy"
  end

(* Returns true if some vertex has increased. *)
let rec process_strategy
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    ~(depth:int)
    (strategy:('vertex,'hedge) strategy)
    :
    bool
    =
  assert(depth>=2);
  let info = PSHGraph.info graph in
  let (it,sstrategy) = strategy in
  let growing = ref false in
  let loop = ref true in
  let counter = ref 0 in

  let rec parcours widening = function
    | [] -> ()
    | elt::rest ->
	let res =
	  begin match elt with
	  | Ilist.Atome(strategy_vertex) ->
	      if PHashhe.mem info.iworkvertex strategy_vertex.vertex then
		process_vertex manager graph ~widening strategy_vertex
	      else
		false
	  | Ilist.List(strategy) ->
	      process_strategy manager graph ~depth:(depth+1) strategy
	  end
	in
	growing := !growing || res;
	loop := !loop || res;
	parcours widening rest;
  in

  if manager.print_component then s_print_intro ~depth manager graph strategy;
  while !loop do
    loop := false;
    incr counter;
    let widening = (!counter >= it.widening_start) in
    parcours widening sstrategy;
    if not !loop && depth>=3 then begin
      (* if Bourdoncle technique, check working sets *)
      try
	Ilist.iter
	  (begin fun _ _ strategy_vertex ->
	    if PHashhe.mem
	      info.iworkvertex strategy_vertex.vertex
	    then begin
	      loop := true; raise Exit
	    end
	  end)
	  strategy
      with
      Exit -> ()
    end
    ;
    if !loop && manager.print_step then s_print_step manager graph strategy counter loop;
  done;
  if manager.print_component then s_print_result ~depth manager graph strategy;
  it.ascending_nb <- it.ascending_nb + !counter;

  !growing

(*  ********************************************************************** *)
(** {2 Process the toplevel strategy} *)
(*  ********************************************************************** *)

(* Returns [(growing,reducing)] *)
let process_toplevel_strategy
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (strategy:('vertex,'hedge) strategy)
    :
    bool * bool
    =
  let info = PSHGraph.info graph in
  let (it,sstrategy) = strategy in
  let ggrowing = ref false in
  let greducing = ref false in
  let rec parcours = function
    | [] -> ()
    | elt::rest ->
	begin match elt with
	| Ilist.Atome(strategy_vertex) ->
	    if PHashhe.mem info.iworkvertex strategy_vertex.vertex then
	      let growing =
		process_vertex manager graph ~widening:false strategy_vertex
	      in
	      ggrowing := !ggrowing || growing
	| Ilist.List(strategy) ->
	    let growing =
	      process_strategy manager graph ~depth:2 strategy
	    in
	    ggrowing := !ggrowing || growing;
	    let reducing =
	      if growing then
		(* Descending *)
		descend manager graph strategy
	      else
		false
	    in
	    greducing := !greducing || reducing
	end;
	parcours rest;
  in
  if manager.print_component then tops_print_intro manager graph strategy;
  if manager.print_workingsets then fprintf manager.print_fmt "%a@ " (print_workingsets manager) graph;

  parcours sstrategy;

  if manager.print_component then tops_print_result manager graph strategy;
  it.ascending_nb <- 1;
  it.descending_nb <- min 1 it.widening_descend;
  it.descending_stable <- not !greducing;

  info.iascending <-
    FixpointType.ilist_map_condense
    (fun it -> { nb = it.ascending_nb; stable = true; })
    strategy;
  info.idescending <-
    FixpointType.ilist_map_condense
    (fun it -> { nb = it.descending_nb; stable = it.descending_stable; })
    (Ilist.flatten ~depth:2 strategy)
  ;

  (!ggrowing,!greducing)


(*  ********************************************************************** *)
(** {2 Standard analysis} *)
(*  ********************************************************************** *)

let output_of_graph graph =
    PSHGraph.copy
      (fun vertex attrvertex -> attrvertex.reach)
      (fun hedge attrhedge -> attrhedge.arc)
      (fun info -> {
	time = !(info.itime);
	ascending = info.iascending;
	descending = info.idescending;
      })
      graph

let analysis
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (input:('vertex,'hedge,'a,'b,'c) PSHGraph.t)
    (sinit:'vertex PSette.t)
    (strategy:('vertex,'hedge) strategy)
    :
    ('vertex,'hedge,'abstract,'arc) output
    =
  if manager.print_analysis then begin
    fprintf manager.print_fmt "*** Analysis...@.";
  end;
  let graph = init manager input sinit in
  let info = PSHGraph.info graph in
  Time.wrap_duration_add info.itime (begin fun () ->
    let (_,reducing) = process_toplevel_strategy manager graph strategy in
    let info = PSHGraph.info graph in
    if manager.print_analysis then
      fprintf manager.print_fmt "... in@.    %a ascending iterations@.    %a descending iterations@.    stabilization:%b@.***@."
	print_stat_iteration_ilist info.iascending
	print_stat_iteration_ilist info.idescending
	(not reducing)
    ;
  end)
  ;
  if manager.print_analysis && manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~title:"Result"
  end
  ;
  output_of_graph graph
