(* Bertrand Jeannet, INRIA. This file is released under LGPL license. *)

(** Guided fixpoint analysis of an equation system *)

(** Technique of [Gopand and Reps, SAS'07]  *)

open Format
open FixpointType

(** Compute set of backedges *)
(*
let strategy_backedge
    (graph:('vertex,'hedge,'e,'f,'g) PSHGraph.t)
    (strategy:('vertex,'hedge) strategy)
    :
    (backedge:('hedge,unit) PHashhe.t)
    =
  let backedge = PHashhe.create_compare graph.PSHGraph.compare.PSHGraph.hashh 7 in
  let seen = ref (PSette.empty graph.PSHGraph.compare.PSHGraph.comparev) in
  Ilist.iter
    (begin fun _ svertex ->
      seen := PSette.add svertex.vertex !seen;
      List.iter
	(begin fun hedge ->
	  let spred = PSHGraph.predvertex graph hedge in
	  let is_backedge =
	    Array.fold_left
	      (begin fun is_backedge vertex ->
		is_backedge || not (PSette.mem vertex !seen)
	      end)
	      false
	      spred
	  in
	  if is_backedge then PHashhe.replace backedge hedge
	end)
	svertex.hedges;
    end)
    strategy
  ;
  backedge
*)

(** Collect newly active hyperedges and
    update working set for vertices and hyperedges *)
let add_active_hedges
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (graph:('vertex,'hedge,'abstract,'arc) graph)
    (activehedge:('hedge,unit) PHashhe.t)
    :
    bool
    =
  let info = PSHGraph.info graph in
  let change =
    PSHGraph.fold_hedge graph
    (begin fun hedge attrhedge ~pred ~succ change ->
      if not (PHashhe.mem activehedge hedge) then begin
	let active =
	  if attrhedge.aempty then begin
	    let tpredvertex = PSHGraph.predvertex graph hedge in
	    if FixpointStd.is_tvertex graph tpredvertex then begin
	      let treach = FixpointStd.treach_of_tvertex ~descend:false graph tpredvertex in
	      let (arc,post) = manager.apply hedge treach in
	      let succvertex = PSHGraph.succvertex graph hedge in
	      assert ((Array.length succvertex)=1);
	      attrhedge.aempty <- manager.is_bottom succvertex.(0) post;
	      not attrhedge.aempty
	    end
	    else
	      false
	  end
	  else
	    true
	in
	if active then begin
	  PHashhe.replace activehedge hedge ();
	  if manager.accumulate then PHashhe.replace info.iworkhedge hedge ();
	  let succvertex = PSHGraph.succvertex graph hedge in
	  assert ((Array.length succvertex)=1);
	  PHashhe.replace info.iworkvertex succvertex.(0) ();
	end;
	change || active
      end
      else
	change
    end)
    false
  in
  change

let analysis
    (manager:('vertex,'hedge,'abstract,'arc) manager)
    (input:('vertex,'hedge,'e,'f,'g) PSHGraph.t)
    (sinit:'vertex PSette.t)
    (make_strategy: ('hedge -> bool) -> ('vertex,'hedge) strategy)
    :
    ('vertex,'hedge,'abstract,'arc) output
    =
  if manager.print_analysis then begin
    fprintf manager.print_fmt "*** Analysis...@."
  end;
  (* Compute acyclic strategy *)
  let strategy_flat = Ilist.flatten (make_strategy (fun _ -> true)) in
  let graph = FixpointStd.init manager input sinit in
  let info = PSHGraph.info graph in
  let loop = ref true in
  let first = ref true in
  let activehedge = PHashhe.create_compare graph.PSHGraph.compare.PSHGraph.hashh 7 in
  Time.wrap_duration_add info.itime (begin fun () ->
    (* (0) Initialization: propagation without backedges *)
    loop := true;
    (* (1) loop *)
    while !loop do
      if manager.print_analysis then begin
	fprintf manager.print_fmt "*** Propagation...@."
      end;
      ignore (FixpointStd.process_toplevel_strategy manager graph strategy_flat);
      if !first then begin
	first := false;
	PSHGraph.iter_hedge graph
	  (begin fun hedge attrhedge ~pred ~succ ->
	    if not attrhedge.aempty then
	      PHashhe.add activehedge hedge ()
	  end)
      end;
      let change =
	(if manager.accumulate then
	  (PHashhe.length info.iworkhedge) > 0
	else
	  true)
	&& add_active_hedges manager graph activehedge
      in
      if change then begin
	let strategy = make_strategy (PHashhe.mem activehedge) in
	if manager.print_analysis then begin
	  fprintf manager.print_fmt "*** Fixpoint...@."
	end;
	ignore
	  (FixpointStd.process_toplevel_strategy manager graph strategy);
	loop :=
	  if manager.accumulate then
	    (PHashhe.length info.iworkhedge) > 0
	  else
	    true
	;
      end
      else
	loop := false
    done;
  end)
  ;
  if manager.print_analysis && manager.dot_fmt<>None then begin
    dot_graph manager graph
      ~title:"Result"
  end
  ;
 FixpointStd.output_of_graph graph
