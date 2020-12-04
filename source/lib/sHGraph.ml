(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Hypergraphs with side-effect semantics *)

(** We implement here oriented hypergraphs. The implementation is
   done by side-effect. *)

open Format

let array_forall f tab =
  let res = ref true in
  for i=0 to pred (Array.length tab) do
    res := !res && f tab.(i)
  done;
  !res

let array_exists f tab =
  let res = ref false in
  for i=0 to pred (Array.length tab) do
    res := !res || f tab.(i)
  done;
  !res

(* *********************************************************************** *)
(** Types *)
(* *********************************************************************** *)

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
type ('a,'b, 'c, 'd, 'e) t = ('a,'b, 'c, 'd, 'e) graph

type 'a priority =
  | Filter of ('a -> bool)
  | Priority of ('a -> int)

let compare_priority (p:'a -> int)
  :
  'a -> 'a -> int
  =
  fun x y ->
    let
      x = p x and
      y = p y
    in
    Pervasives.compare y x

let map_priority cmp hedge_dummy = function
  | None -> None
  | Some(Filter p) ->
      Some(Filter(fun h -> if cmp.hashh.Hashhe.equal h hedge_dummy then true else p h))
  | Some(Priority p) ->
      Some(Priority(fun h -> if cmp.hashh.Hashhe.equal h hedge_dummy then 0 else p h))

(* ======================================================================= *)
(* Iterators *)
(* ======================================================================= *)

let iter_vertex g f =
  Hashhe.iter
    (fun v vertex_n -> f v vertex_n.attrvertex ~pred:vertex_n.predhedge ~succ:vertex_n.succhedge)
    g.vertex
let iter_hedge g f =
  Hashhe.iter
    (fun n hedge_n -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex)
    g.hedge

let fold_vertex g f res =
  Hashhe.fold
    (fun v vertex_n res -> f v vertex_n.attrvertex ~pred:vertex_n.predhedge ~succ:vertex_n.succhedge  res)
    g.vertex res
let fold_hedge g f res =
  Hashhe.fold
    (fun n hedge_n res -> f n hedge_n.attrhedge ~pred:hedge_n.predvertex ~succ:hedge_n.succvertex res)
    g.hedge res

(* ======================================================================= *)
(* Copy *)
(* ======================================================================= *)

let map g map_attrvertex map_attrhedge map_info = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> { vertex_n with
	attrvertex = map_attrvertex v vertex_n.attrvertex;
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> { hedge_n with
	attrhedge = map_attrhedge h hedge_n.attrhedge;
      })
      g.hedge
  end;
  info = map_info g.info
}

let copy copy_attrvertex copy_attrhedge copy_info g = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> { vertex_n with
	attrvertex = copy_attrvertex v vertex_n.attrvertex;
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> { hedge_n with
	attrhedge = copy_attrhedge h hedge_n.attrhedge;
      })
      g.hedge
  end;
  info = copy_info g.info
}

let transpose copy_attrvertex copy_attrhedge copy_info g = {
  vertex = begin
    Hashhe.map
      (fun v vertex_n -> {
	attrvertex = copy_attrvertex v vertex_n.attrvertex;
	predhedge = vertex_n.succhedge;
	succhedge = vertex_n.predhedge
      })
      g.vertex
  end;
  hedge = begin
    Hashhe.map
      (fun h hedge_n -> {
	attrhedge = copy_attrhedge h hedge_n.attrhedge;
	predvertex = hedge_n.succvertex;
	succvertex = hedge_n.predvertex
      })
      g.hedge
  end;
  info = copy_info g.info
}

(* *********************************************************************** *)
(* Generic interface *)
(* *********************************************************************** *)

module Compare = struct
  let vertex_n cmp g v = Hashhe.Compare.find cmp.hashv g.vertex v
  let hedge_n cmp g h = Hashhe.Compare.find cmp.hashh g.hedge h
  let attrvertex cmp g v =
    (vertex_n cmp g v).attrvertex
  let attrhedge cmp g h =
    ( hedge_n cmp g h).attrhedge
  let is_vertex cmp g v =
    Hashhe.Compare.mem cmp.hashv g.vertex v
  let is_hedge cmp g h =
    Hashhe.Compare.mem cmp.hashh g.hedge h
  let succhedge cmp g v = (vertex_n cmp g v).succhedge
  let predhedge cmp g v = (vertex_n cmp g v).predhedge
  let succvertex cmp g h = (hedge_n cmp g h).succvertex
  let predvertex cmp g h = (hedge_n cmp g h).predvertex

  let succ_vertex cmp g v =
    let succhedge = succhedge cmp g v in
    let res = ref Sette.empty in
    Sette.iter
      (begin fun h ->
	let succvertex = succvertex cmp g h in
	Array.iter
	  (fun v -> res := Sette.Compare.add cmp.comparev v !res)
	  succvertex
      end)
      succhedge;
    !res

  let pred_vertex cmp g v =
    let predhedge = predhedge cmp g v in
    let res = ref Sette.empty in
    Sette.iter
      (begin fun h ->
	let predvertex = predvertex cmp g h in
	Array.iter
	  (fun v -> res := Sette.Compare.add cmp.comparev v !res)
	  predvertex
      end)
      predhedge;
    !res

  let add_vertex cmp g v attrvertex =
    if is_vertex cmp g v then
      failwith "SHGraph.add_vertex: the vertex already exists (use replace_vertex)"
    ;
    Hashhe.Compare.add cmp.hashv g.vertex
      v { attrvertex=attrvertex; succhedge=Sette.empty; predhedge=Sette.empty }

  let add_hedge cmp g h attrhedge ~pred ~succ =
    if is_hedge cmp g h then
      failwith "SHGraph.add_hedge: the hedge already exists (use replace_hedge)"
    ;
    begin try
      Array.iter
	(fun v ->
	  let vertex_n =  vertex_n cmp g v in
	  vertex_n.succhedge <- Sette.Compare.add cmp.compareh h vertex_n.succhedge;
	)
	pred;
      Array.iter
	(fun v ->
	  let vertex_n =  vertex_n cmp g v in
	  vertex_n.predhedge <- Sette.Compare.add cmp.compareh h vertex_n.predhedge;
	)
	succ;
    with Not_found ->
      failwith "SHGraph.add_hedge: origin vertex and/or destination hedge doesn't already exist"
    end;
    Hashhe.Compare.add cmp.hashh g.hedge
      h { attrhedge=attrhedge; predvertex=pred; succvertex=succ }

  let replace_attrvertex cmp g v attrvertex =
    let vertex_n = Hashhe.Compare.find cmp.hashv g.vertex v in
    let vertex_n = { vertex_n with attrvertex=attrvertex } in
    Hashhe.Compare.replace cmp.hashv g.vertex v vertex_n

  let replace_attrhedge cmp g h attrhedge =
    let hedge_n = Hashhe.Compare.find cmp.hashh g.hedge h in
    let hedge_n = { hedge_n with attrhedge=attrhedge } in
    Hashhe.Compare.replace cmp.hashh g.hedge h hedge_n

  let remove_hedge cmp g h =
    try
      let hedge_n = hedge_n cmp g h in
      Array.iter
	(begin fun v ->
	  let vertex_n = vertex_n cmp g v in
	  vertex_n.succhedge <- Sette.Compare.remove cmp.compareh h vertex_n.succhedge
	end)
	hedge_n.predvertex;
      Array.iter
	(begin fun v ->
	  let vertex_n = vertex_n cmp g v in
	  vertex_n.predhedge <- Sette.Compare.remove cmp.compareh h vertex_n.predhedge
	end)
	hedge_n.succvertex;
      Hashhe.Compare.remove cmp.hashh g.hedge h;
    with Not_found ->
      ()

  let remove_vertex cmp g v =
    try
      let vertex_n = vertex_n cmp g v in
      Sette.iter (fun h -> remove_hedge cmp g h) vertex_n.predhedge;
      Sette.iter (fun h -> remove_hedge cmp g h) vertex_n.succhedge;
      Hashhe.Compare.remove cmp.hashv g.vertex v
    with Not_found ->
      ()

  (* ======================================================================= *)
  (* Very internal functions *)
  (* ======================================================================= *)

  let add_dummy_forward cmp (vertex_dummy:'a) (hedge_dummy:'b) g (root:'a Sette.t) : unit =
    assert (root<>Sette.empty);
    Hashhe.Compare.add cmp.hashv g.vertex vertex_dummy
      {
	attrvertex=Obj.magic 0;
	succhedge=Sette.singleton hedge_dummy;
	predhedge=Sette.empty
      };
    Hashhe.Compare.add cmp.hashh g.hedge hedge_dummy
      {
	attrhedge=Obj.magic 0;
	succvertex=begin
	  let tab = Array.make (Sette.cardinal root) (Sette.choose root) in
	  let i = ref 0 in
	  Sette.iter (fun v -> tab.(!i) <- v; incr i) root;
	  tab
	end;
	predvertex=[|vertex_dummy|]
      };
    ()

  let rem_dummy cmp (vertex_dummy:'a) (hedge_dummy:'b) g : unit =
    Hashhe.Compare.remove cmp.hashv g.vertex vertex_dummy;
    Hashhe.Compare.remove cmp.hashh g.hedge hedge_dummy

  let topological_sort
    cmp
    ?priority
    (g:('a,'b,'c,'d,'e) t)
    (root:'a)
    =
    let hash = Hashhe.create 23 in

    let rec visit res v =
      if Hashhe.Compare.mem cmp.hashv hash v then
	res
      else begin
	Hashhe.Compare.add cmp.hashv hash v ();
	let succhedges = succhedge cmp g v in
	let nres = v::res in
	match priority with
	| None -> Sette.fold explore succhedges nres
	| Some(Filter(p)) ->
	    Sette.fold
	    (begin fun h res -> if p h then explore h res else res end)
	    succhedges nres
	| Some(Priority(p)) ->
	    let lhedges =
	      Sette.fold
		(begin fun h res -> if p h >= 0 then h::res else res end)
		succhedges []
	    in
	    let lhedges = List.sort (compare_priority p) lhedges in
	    List.fold_left
	      (begin fun res h -> explore h res end)
	      nres
	      lhedges
      end

    and explore (hedge:'b) (res:'a list) : 'a list
      =
      let hedge_n = hedge_n cmp g hedge in
      let cond =
	array_forall
	  (Hashhe.Compare.mem cmp.hashv hash)
	  hedge_n.predvertex
      in
      if cond then
	Array.fold_left
	  visit
	  res
	  hedge_n.succvertex
      else
	res

    in
    let res = visit [] root in
    List.rev res

  let topological_sort_multi cmp vertex_dummy hedge_dummy ?priority g root =
    add_dummy_forward cmp vertex_dummy hedge_dummy g root;
    let res =
      topological_sort cmp
	?priority:(map_priority cmp hedge_dummy priority)
	g vertex_dummy
    in
    rem_dummy cmp vertex_dummy hedge_dummy g;
    List.tl res

  let reachable
    cmp
    ?filter
    (g:('a,'b,'c,'d,'e) t)
    (root:'a)
    =
    let hashv = Hashhe.create 23 in
    let hashh = Hashhe.create 23 in
    let rec visit v =
      if not (Hashhe.Compare.mem cmp.hashv hashv v) then begin
	Hashhe.Compare.add cmp.hashv hashv v ();
	Sette.iter
	  (begin fun h ->
	    if
	      begin match filter with
	      | None -> true
	      | Some p -> p h
	      end
	      &&
	      not (Hashhe.Compare.mem cmp.hashh hashh h)
	    then begin
	      let hedge_n = hedge_n cmp g h in
	      let cond =
		array_forall
		  (Hashhe.Compare.mem cmp.hashv hashv)
		  hedge_n.predvertex
	      in
	      if cond then begin
		Hashhe.Compare.add cmp.hashh hashh h ();
		Array.iter
		  visit
		  hedge_n.succvertex
	      end
	    end
	  end)
	  (succhedge cmp g v)
      end
    in
    visit root;
    let setv =
      fold_vertex g
	(begin fun v _ ~pred:_ ~succ:_ res ->
	  if not (Hashhe.Compare.mem cmp.hashv hashv v)
	  then Sette.Compare.add cmp.comparev v res
	  else res
	end)
	Sette.empty
    and setn =
      fold_hedge g
	(begin fun h _ ~pred:_ ~succ:_ res ->
	  if not (Hashhe.Compare.mem cmp.hashh hashh h)
	  then Sette.Compare.add cmp.compareh h res
	  else res
	end)
	Sette.empty
    in
    (setv,setn)

  let reachable_multi cmp vertex_dummy hedge_dummy ?filter g root =
    add_dummy_forward cmp vertex_dummy hedge_dummy g root;
    let res =
      reachable cmp
	?filter:begin match filter with
	| None -> None
	| Some p -> Some(fun h -> if cmp.hashh.Hashhe.equal h hedge_dummy then true else p h)
	end
	g vertex_dummy
    in
    rem_dummy cmp vertex_dummy hedge_dummy g;
    let (vertices,hedges) = res in
    (Sette.Compare.remove cmp.comparev vertex_dummy vertices,
     Sette.Compare.remove cmp.compareh hedge_dummy hedges)

  let cfc
    cmp
    ?priority
    g root
    =
    let
      num      = ref(-1) and
      partition = ref [] and
      pile     = Stack.create() and
      hash     = Hashhe.map (fun _ _ -> min_int) g.vertex
    in
    let rec visit sommet
      =
      Stack.push sommet pile;
      incr num;
      let num_sommet = !num in
      Hashhe.Compare.replace cmp.hashv hash sommet num_sommet;
      let succhedges = succhedge cmp g sommet in
      let head =
	match priority with
	| None ->
	    Sette.fold compute_head succhedges !num
	| Some(Filter p) ->
	    Sette.fold
	    (begin fun h head ->
	      if p h then compute_head h head else head
	    end)
	    succhedges !num
	| Some(Priority p) ->
	    let lhedges =
	      Sette.fold
		(begin fun h res -> if p h >= 0 then h::res else res end)
		succhedges []
	    in
	    let lhedges = List.sort (compare_priority p) lhedges in
	    List.fold_left (fun head h -> compute_head h head) !num lhedges
      in
      if head = num_sommet then
	begin
	  let element = ref (Stack.pop pile) in
	  let composante = ref [!element] in
	  Hashhe.Compare.replace cmp.hashv hash !element max_int;
	  while not (cmp.hashv.Hashhe.equal !element sommet) do
	    element := Stack.pop pile;
	    Hashhe.Compare.replace cmp.hashv hash !element max_int;
	    composante := !element :: !composante
	  done;
	  partition := !composante :: !partition
	end;
      head
    and compute_head h head
      =
      let hedge_n = hedge_n cmp g h in
      let cond =
	array_exists
	  (fun v -> Hashhe.Compare.find cmp.hashv hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.fold_left
	  (begin fun head succ ->
	    let dfn = Hashhe.Compare.find cmp.hashv hash succ in
	    let m = if dfn=min_int then (visit succ) else dfn in
	    min m head
	  end)
	  head
	  hedge_n.succvertex
      end
      else
	head
    in

    let _ = visit root in
    !partition

  let cfc_multi cmp ?priority vertex_dummy hedge_dummy g root =
    add_dummy_forward cmp vertex_dummy hedge_dummy g root;
    let res = cfc cmp
      ?priority:(map_priority cmp hedge_dummy priority)
      g vertex_dummy
    in
    rem_dummy cmp vertex_dummy hedge_dummy g;
    begin match res with
    | (x::r)::l when (cmp.hashv.Hashhe.equal x vertex_dummy) -> r::l
    | _ -> failwith "cfc_multi"
    end

  (* ======================================================================= *)
  (* Strongly Connected Sub-Components *)
  (* ======================================================================= *)
  let scfc
    cmp
    ?priority
    g
    root
    =
    let num     = ref(-1) and
      pile    = Stack.create() and
      hash    = Hashhe.map (fun _ _ -> min_int) g.vertex
    in
    let rec composante_aux partition h
      =
      let hedge_n = hedge_n cmp g h in
      let cond =
	array_exists
	  (fun v -> Hashhe.Compare.find cmp.hashv hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.iter
	  (begin fun succ ->
	    if Hashhe.Compare.find cmp.hashv hash succ = min_int then begin
	      ignore (visit succ partition)
	    end
	  end)
	  hedge_n.succvertex
      end
    and composante sommet
      =
      let partition = ref [] in
      let succhedges = succhedge cmp g sommet in
      begin match priority with
      | None ->
	  Sette.iter (composante_aux partition) succhedges
      | Some(Filter p) ->
	  Sette.iter
	  (begin fun h ->
	    if p h then composante_aux partition h
	  end)
	  succhedges
      | Some(Priority p) ->
	  let lhedges =
	    Sette.fold
	      (begin fun h res -> if p h >=0 then h::res else res end)
	      succhedges []
	  in
	  let lhedges = List.sort (compare_priority p) lhedges in
	  List.iter (composante_aux partition) lhedges
      end;
      Ilist.Atome(sommet)::(!partition)

    and visit sommet partition
      =
      Stack.push sommet pile;
      incr num;
      let num_sommet = !num in
      Hashhe.Compare.replace cmp.hashv hash sommet num_sommet;
      let head = ref !num and loop = ref false in
      let succhedges = succhedge cmp g sommet in
      begin match priority with
      | None ->
	  Sette.iter (compute_head partition head loop) succhedges
      | Some(Filter p) ->
	  Sette.iter
	  (begin fun h ->
	    if p h then (compute_head partition head loop) h
	  end)
	  succhedges
      | Some(Priority p) ->
	  let lhedges =
	    Sette.fold
	      (begin fun h res -> if p h >= 0 then h::res else res end)
	      succhedges []
	  in
	  let lhedges = List.sort (compare_priority p) lhedges in
	  List.iter (compute_head partition head loop) lhedges
      end
      ;
      if !head = num_sommet then
	begin
	  Hashhe.Compare.replace cmp.hashv hash sommet max_int;
	  let element = ref (Stack.pop pile) in
	  if !loop then
	    begin
	      while not (cmp.hashv.Hashhe.equal !element sommet) do
		Hashhe.Compare.replace cmp.hashv hash !element min_int;
		element := Stack.pop pile
	      done;
	      let component = composante sommet in
	      partition := Ilist.List((),component)::(!partition)
	    end
	  else
	    partition := Ilist.Atome(sommet)::(!partition)
	end;
      !head

    and compute_head partition head loop h
      =
      let hedge_n = hedge_n cmp g h in
      let cond =
	array_exists
	  (fun v -> Hashhe.Compare.find cmp.hashv hash v > min_int)
	  hedge_n.predvertex
      in
      if cond then begin
	Array.iter
	  (begin fun succ ->
	    let dfn = Hashhe.Compare.find cmp.hashv hash succ in
	    let m =
	      if dfn=min_int
	      then (visit succ partition)
	      else dfn
	    in
	    if m <= !head then begin loop := true; head := m end
	  end)
	  hedge_n.succvertex
      end
    in

    let partition = ref [] in
    let _ = visit root partition in
    ((),!partition)

  let scfc_multi cmp vertex_dummy hedge_dummy ?priority g root =
    add_dummy_forward cmp vertex_dummy hedge_dummy g root;
    let res = scfc cmp
      ?priority:(map_priority cmp hedge_dummy priority)
      g vertex_dummy
    in
    rem_dummy cmp vertex_dummy hedge_dummy g;
    begin match res with
    | (_,Ilist.Atome(x)::l) when (cmp.hashv.Hashhe.equal x vertex_dummy) -> ((),l)
    | _ -> failwith "hGraph.ml: scfc_multi"
    end

  let print
    cmp
    print_vertex print_hedge print_attrvertex print_attrhedge print_info
    formatter g
    =
    let printv v =
      let vertex_n = vertex_n cmp g v in
      fprintf formatter
	"{ @[<hov>v = %a,@ attrv = %a,@ predh = %a,@ succh = %a@] }@ "
	print_vertex v
	print_attrvertex vertex_n.attrvertex
	(Sette.print print_hedge) vertex_n.predhedge
	(Sette.print print_hedge) vertex_n.succhedge
    and printh h =
      let hedge_n = hedge_n cmp g h in
      fprintf formatter
	"{ @[<hov>h = %a,@ attrh = %a,@ predv = %a,@ succv = %a@] }@ "
	print_hedge h
	print_attrhedge hedge_n.attrhedge
	(Print.array print_vertex) hedge_n.predvertex
	(Print.array print_vertex) hedge_n.succvertex
    in

    (* Build the set of vertices and hedges and sort it *)
    fprintf formatter "[ @[<v>";
    let vertices =
      Hashhe.fold (fun v _ res -> Sette.Compare.add cmp.comparev v res) g.vertex Sette.empty
    in
    Sette.iter printv vertices;
    let hedges =
      Hashhe.fold (fun n _ res -> Sette.Compare.add cmp.compareh n res) g.hedge Sette.empty
    in
    Sette.iter printh hedges;
    fprintf formatter "info = %a@ " print_info g.info;

    fprintf formatter "@] ]";
    ()

  let min cmp g =
    Hashhe.fold
      (fun v vertex_n res ->
	if vertex_n.predhedge = Sette.empty
	then Sette.Compare.add cmp.comparev v res
	else res)
      g.vertex Sette.empty

  let max cmp g =
    Hashhe.fold
      (fun v vertex_n res ->
	if vertex_n.succhedge = Sette.empty
	then Sette.Compare.add cmp.comparev v res
	else res)
      g.vertex Sette.empty
end

(* *********************************************************************** *)
(* Polymorphic interface *)
(* *********************************************************************** *)

let stdcompare =
  let cmp x y = Pervasives.compare x y in
  {
    hashv = Hashhe.stdcompare;
    hashh = Hashhe.stdcompare;
    comparev = cmp;
    compareh = cmp
  }

let create size info = {
  vertex = Hashhe.create size;
  hedge = Hashhe.create size;
  info = info;
}

let clear g =
  Hashhe.clear g.vertex;
  Hashhe.clear g.hedge;
  ()

(* ======================================================================= *)
(* Size *)
(* ======================================================================= *)
let size_vertex g =
  Hashhe.length g.vertex
let size_hedge g =
  Hashhe.length g.hedge
let size_edgevh g =
  let n = ref 0 in
  Hashhe.iter
    (fun _ vertex_n ->
      n := !n + Sette.cardinal vertex_n.succhedge)
    g.vertex;
  !n
let size_edgehv g =
  let n = ref 0 in
  Hashhe.iter
    (fun _ hedge_n ->
      n := !n + Array.length hedge_n.succvertex)
    g.hedge;
  !n
let size g =
  (size_vertex g, size_hedge g, size_edgevh g, size_edgehv g)

(* ======================================================================= *)
(* Access functions *)
(* ======================================================================= *)
let attrvertex g v = Compare.attrvertex stdcompare g v
let attrhedge g h = Compare.attrhedge stdcompare g h
let info g = g.info

(* ======================================================================= *)
(* Test functions *)
(* ======================================================================= *)
let is_vertex g v = Compare.is_vertex stdcompare g v
let is_hedge g h =  Compare.is_hedge stdcompare g h
let is_empty g =
  try
    Hashhe.iter
      (fun _ _ ->raise Exit)
      g.vertex;
    true
  with Exit ->
    false

(* ======================================================================= *)
(* Successors and predecessors *)
(* ======================================================================= *)
let succhedge g v = Compare.succhedge stdcompare g v
let predhedge g v = Compare.predhedge stdcompare g v
let succvertex g h = Compare.succvertex stdcompare g h
let predvertex g h = Compare.predvertex stdcompare g h
let succ_vertex g v = Compare.succ_vertex stdcompare g v
let pred_vertex g v = Compare.pred_vertex stdcompare g v

(* ======================================================================= *)
(* Addition of nodes and edges *)
(* ======================================================================= *)
let add_vertex g v attrvertex =
  Compare.add_vertex stdcompare g v attrvertex

let add_hedge g h attrhedge ~pred ~succ =
  Compare.add_hedge stdcompare g h attrhedge ~pred ~succ

let replace_attrvertex g v attrvertex =
  Compare.replace_attrvertex stdcompare g v attrvertex
let replace_attrhedge g h attrhedge =
  Compare.replace_attrhedge stdcompare g h attrhedge

(* ======================================================================= *)
(* Removal of nodes and edges *)
(* ======================================================================= *)

let remove_hedge g h = Compare.remove_hedge stdcompare g h
let remove_vertex g v = Compare.remove_vertex stdcompare g v

(* ======================================================================= *)
(* Topological sort *)
(* ======================================================================= *)

let topological_sort ?priority g root =
  Compare.topological_sort stdcompare ?priority g root

let topological_sort_multi vertex_dummy hedge_dummy ?priority g root =
  Compare.topological_sort_multi stdcompare vertex_dummy hedge_dummy ?priority g root

(* ======================================================================= *)
(* Reachability/Coreachability *)
(* ======================================================================= *)

let reachable ?filter g root =
  Compare.reachable stdcompare ?filter g root

let reachable_multi vertex_dummy hedge_dummy ?filter g root =
  Compare.reachable_multi stdcompare vertex_dummy hedge_dummy ?filter g root

(* ======================================================================= *)
(* Strongly Connected Components *)
(* ======================================================================= *)

let cfc ?priority g root =
  Compare.cfc stdcompare ?priority g root

let cfc_multi vertex_dummy hedge_dummy ?priority g root =
  Compare.cfc_multi stdcompare vertex_dummy hedge_dummy ?priority g root

(* ======================================================================= *)
(* Strongly Connected Sub-Components *)
(* ======================================================================= *)

let scfc ?priority g root =
  Compare.scfc stdcompare ?priority g root

let scfc_multi vertex_dummy hedge_dummy ?priority g root =
  Compare.scfc_multi stdcompare vertex_dummy hedge_dummy ?priority g root

(* ======================================================================= *)
(* Printing *)
(* ======================================================================= *)

let print pv ph pattrv pattrh pinfo fmt h =
  Compare.print stdcompare pv ph pattrv pattrh pinfo fmt h

let print_dot
    ?(style:string="")
    ?(titlestyle:string="shape=plaintext,style=bold,style=filled,fontsize=12")
    ?(vertexstyle:string="shape=box,fontsize=12")
    ?(hedgestyle:string="shape=ellipse,fontsize=12")
    ?fvertexstyle
    ?fhedgestyle
    ?(title:string="")
    print_vertex print_hedge print_attrvertex print_attrhedge
    fmt g
    =
  fprintf fmt "digraph G {@.  @[<v>%s@ " style;
  if title<>"" then
    fprintf fmt "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
  Hashhe.iter
    (begin fun vertex vertex_n ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
	print_vertex vertex
	(match fvertexstyle with
	| Some f -> f vertex
	| None -> vertexstyle)
	(fun fmt -> print_attrvertex fmt vertex vertex_n.attrvertex);
    end)
    g.vertex;
  Hashhe.iter
    (begin fun hedge hedge_n ->
      fprintf fmt "%a [%s,label=\"%t\"];@ "
	print_hedge hedge
	(match fhedgestyle with
	| Some f -> f hedge
	| None -> hedgestyle)
	(fun fmt -> print_attrhedge fmt hedge hedge_n.attrhedge);
    end)
    g.hedge;
  Hashhe.iter
    (begin fun hedge hedge_n ->
      Array.iter
	(begin fun pred ->
	  fprintf fmt "%a -> %a;@ "
	    print_vertex pred print_hedge hedge
	end)
	hedge_n.predvertex;
      Array.iter
	(begin fun succ ->
	  fprintf fmt "%a -> %a;@ "
	    print_hedge hedge print_vertex succ
	end)
	hedge_n.succvertex
    end)
    g.hedge
  ;
  fprintf fmt "@]@.}@.";
  ()

(* *********************************************************************** *)
(* Functor interface *)
(* *********************************************************************** *)

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

module Make(T : T) : (S with type vertex=T.vertex
			and type hedge=T.hedge
			and module SetV=T.SetV
			and module SetH=T.SetH
			and module HashV=T.HashV
			and module HashH=T.HashH)= struct

  type vertex = T.vertex
  type hedge = T.hedge
  let vertex_dummy = T.vertex_dummy
  let hedge_dummy = T.hedge_dummy
  module SetV = T.SetV
  module SetH = T.SetH
  module HashV = T.HashV
  module HashH = T.HashH

  let stdcompare = {
    hashv = {
      Hashhe.hash = HashV.Hash.hash;
      Hashhe.equal = HashV.Hash.equal
    };
    hashh = {
      Hashhe.hash = HashH.Hash.hash;
      Hashhe.equal = HashH.Hash.equal
    };
    comparev = SetV.Ord.compare;
    compareh = SetH.Ord.compare;
  }

  type ('a,'b,'c) t = (vertex, hedge, 'a,'b,'c) graph

  let create = create
  let clear = clear
  let size_vertex = size_vertex
  let size_hedge = size_hedge
  let size_edgevh = size_edgevh
  let size_edgehv = size_edgehv
  let size = size

  let attrvertex g v = Compare.attrvertex stdcompare g v
  let attrhedge g h = Compare.attrhedge stdcompare g h
  let info = info

  let is_vertex g v = Compare.is_vertex stdcompare g v
  let is_hedge g h =  Compare.is_hedge stdcompare g h
  let is_empty = is_empty

  let succhedge g v = SetH.obj (Compare.succhedge stdcompare g v)
  let predhedge g v = SetH.obj (Compare.predhedge stdcompare g v)
  let succvertex g h = Compare.succvertex stdcompare g h
  let predvertex g h = Compare.predvertex stdcompare g h
  let succ_vertex g v = SetV.obj (Compare.succ_vertex stdcompare g v)
  let pred_vertex g v = SetV.obj (Compare.pred_vertex stdcompare g v)

  let add_vertex g v attrvertex =
    Compare.add_vertex stdcompare g v attrvertex
  let add_hedge g h attrhedge ~pred ~succ =
    Compare.add_hedge stdcompare g h attrhedge ~pred ~succ

  let replace_attrvertex g v attrvertex =
    Compare.replace_attrvertex stdcompare g v attrvertex
  let replace_attrhedge g h attrhedge =
    Compare.replace_attrhedge stdcompare g h attrhedge

  let remove_hedge g h = Compare.remove_hedge stdcompare g h
  let remove_vertex g v = Compare.remove_vertex stdcompare g v

  let iter_vertex = Obj.magic iter_vertex
  let fold_vertex = Obj.magic fold_vertex
  let iter_hedge = Obj.magic iter_hedge
  let fold_hedge = Obj.magic fold_hedge

  let map = Obj.magic map
  let copy = Obj.magic copy
  let transpose = Obj.magic transpose

  let topological_sort ?priority g root =
    Compare.topological_sort stdcompare ?priority g root

  let topological_sort_multi ?priority g root =
    Compare.topological_sort_multi stdcompare vertex_dummy hedge_dummy ?priority g (SetV.repr root)

  let reachable ?filter g root =
    let (setv,seth) = Compare.reachable stdcompare ?filter g root in
    (SetV.obj setv, SetH.obj seth)

  let reachable_multi ?filter g root =
    let (setv,seth) = Compare.reachable_multi stdcompare vertex_dummy hedge_dummy ?filter g (SetV.repr root) in
    (SetV.obj setv, SetH.obj seth)

  let cfc ?priority g root =
    Compare.cfc stdcompare ?priority g root

  let cfc_multi ?priority g root =
    Compare.cfc_multi stdcompare vertex_dummy hedge_dummy ?priority g (SetV.repr root)

  let scfc ?priority g root  =
    Compare.scfc stdcompare ?priority g root

  let scfc_multi ?priority g root =
    Compare.scfc_multi stdcompare vertex_dummy hedge_dummy ?priority g (SetV.repr root)

  let print pv ph pattrv pattrh pinfo fmt h =
    Compare.print stdcompare pv ph pattrv pattrh pinfo fmt h

  let print_dot = print_dot

  let min g = SetV.obj (Compare.min stdcompare g)
  let max g = SetV.obj (Compare.max stdcompare g)

end

let min g = Compare.min stdcompare g
let max g = Compare.max stdcompare g
