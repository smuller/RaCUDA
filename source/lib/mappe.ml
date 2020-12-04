(** Association tables over ordered types *)

type ('a,'b) map =
    Empty
  | Node of ('a,'b) map * 'a * 'b * ('a,'b) map * int

type ('a,'b) t = ('a,'b) map

let is_empty t = t==Empty
let empty = Empty

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
	if height ll >= height lr then
	  create ll lv ld (create lr x d r)
	else begin
	  match lr with
	    Empty -> invalid_arg "Map.bal"
	  | Node(lrl, lrv, lrd, lrr, _)->
	      create (create ll lv ld lrl) lrv lrd (create lrr x d r)
	end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
	if height rr >= height rl then
	  create (create l x d rl) rv rd rr
	else begin
	  match rl with
	    Empty -> invalid_arg "Map.bal"
	  | Node(rll, rlv, rld, rlr, _) ->
	      create (create l x d rll) rlv rld (create rlr rv rd rr)
	    end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

(** Smallest and greatest key of a map *)
let rec min_key = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> x
  | Node(l, x, d, r, _) -> min_key l

let rec max_key = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> x
  | Node(l, x, d, r, _) -> max_key r

let rec min_binding = function
  Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding l

let rec remove_min_binding = function
  Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
  (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

let rec map f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
      fold f l (f v d (fold f r accu))

let rec maptoset = function
  | Empty               -> Sette.Empty
  | Node(l, v, d, r, h) -> Sette.Node(maptoset l, v, maptoset r, h)

let rec mapofset f = function
  | Sette.Empty -> Empty
  | Sette.Node(l,v,r,h) -> Node(mapofset f l,v,f v,mapofset f r,h)

let rec mapi f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

let rec cardinal = function
    Empty -> 0
  | Node(l, v, d, r, _) -> cardinal l + 1 + cardinal r

let rec bindings_aux accu = function
  | Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v,d) :: bindings_aux accu r) l

let bindings s =
  bindings_aux [] s

let choose = min_binding

let print
  ?(first : (unit, Format.formatter, unit) format = ("[@[<hv>" : (unit, Format.formatter, unit) format))
  ?(sep : (unit, Format.formatter, unit) format = (";@ ":(unit, Format.formatter, unit) format))
  ?(last : (unit, Format.formatter, unit) format = ("@]]":(unit, Format.formatter, unit) format))
  ?(firstbind : (unit, Format.formatter, unit) format = ("" : (unit, Format.formatter, unit) format))
  ?(sepbind : (unit, Format.formatter, unit) format = (" => ":(unit, Format.formatter, unit) format))
  ?(lastbind : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  (print_key:Format.formatter -> 'a -> unit)
  (print_data:Format.formatter -> 'b -> unit)
  (formatter:Format.formatter)
  (map:('a,'b) t)
  : unit
  =
  Format.fprintf formatter first;
  let firstitem = ref true in
  iter
    (begin fun key data ->
      if !firstitem then firstitem := false else Format.fprintf formatter sep;
      Format.fprintf formatter firstbind;
      print_key formatter key;
      Format.fprintf formatter sepbind;
      print_data formatter data;
      Format.fprintf formatter lastbind;
    end)
    map;
  Format.fprintf formatter last

module Compare = struct

  let rec add (compare:'a -> 'a -> int) x data = function
    | Empty ->
	Node(Empty, x, data, Empty, 1)
    | Node(l, v, d, r, h) ->
	let c = compare x v in
	if c = 0 then
	  Node(l, x, data, r, h)
	else if c < 0 then
	  bal (add compare x data l) v d r
	else
	  bal l v d (add compare x data r)

  (** Same as create and bal, but no assumptions are made on the relative
     heights of l and r. *)
  let rec join (compare:'a -> 'a -> int) l x d r =
    match (l, r) with
    | (Empty, _) -> add compare x d r
    | (_, Empty) -> add compare x d l
    | (Node(ll, lx, ld, lr, lh), Node(rl, rx, rd, rr, rh)) ->
	if lh > rh + 2 then bal ll lx ld (join compare lr x d r) else
	  if rh > lh + 2 then bal (join compare l x d rl) rx rd rr else
	    create l x d r

  let rec find (compare:'a -> 'a -> int) x = function
   | Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d
      else find compare x (if c < 0 then l else r)

  let rec mem (compare:'a -> 'a -> int) x = function
    | Empty ->
	false
    | Node(l, v, d, r, _) ->
	let c = compare x v in
	c = 0 || mem compare x (if c < 0 then l else r)

  (** Merge two trees l and r into one.  All elements of l must precede the
    elements of r.  No assumption on the heights of l and r. *)
  let concat (compare:'a -> 'a -> int) t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
	let (x,d) = min_binding t2 in
	join compare t1 x d (remove_min_binding t2)

  (** Splitting.  split x s returns a quadruple (l, present, r) where
    - l is the set of bindings of s with keys that are < x
    - r is the set of bindings of s with keys that are > x
    - present is None if s contains no key equal to x,
      or Some(data) if  s contains a key equal to x bound to data
  *)
  let rec split (compare:'a -> 'a -> int) key = function
    | Empty ->
	(Empty, None, Empty)
    | Node(l, x, d, r, _) ->
	let c = compare key x in
	if c = 0 then (l, (Some d), r)
	else if c < 0 then
	  let (ll, pres, rl) = split compare key l in (ll, pres, join compare rl x d r)
	else
	  let (lr, pres, rr) = split compare key r in (join compare l x d lr, pres, rr)

  let rec remove (compare:'a -> 'a -> int) x = function
    | Empty ->
	Empty
    | Node(l, v, d, r, h) ->
	let c = compare x v in
	if c = 0 then
	  merge l r
	else if c < 0 then
	  bal (remove compare x l) v d r
	else
	  bal l v d (remove compare x r)

  let rec addmap (compare:'a -> 'a -> int) m1 m2 =
    match (m1, m2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, x1, d1, r1, h1), Node(l2, x2, d2, r2, h2)) ->
	if h1 >= h2 then
	  if h2 = 1 then add compare x2 d2 m1 else begin
	    let (l2, pres, r2) = split compare x1 m2 in
	    join compare
	      (addmap compare l1 l2)
	      x1
	      (match pres with None -> d1 | Some d2 -> d2)
	      (addmap compare r1 r2)
	  end
	else
	  if h1 = 1 then add compare x1 d1 m2 else begin
	    let (l1, _, r1) = split compare x2 m1 in
	    join compare (addmap compare l1 l2) x2 d2 (addmap compare r1 r2)
	  end

  let common (compare:'a -> 'a -> int) f m1 m2 =
    let rec common m1 m2 =
      match (m1, m2) with
      | (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, x1, d1, r1, _), t2) ->
	  match split compare x1 t2 with
	  | (l2, None, r2) ->
	      concat compare (common l1 l2) (common r1 r2)
	  | (l2, (Some d2), r2) ->
	      join compare (common l1 l2) x1 (f d1 d2) (common r1 r2)
    in
    common m1 m2

  let commoni (compare:'a -> 'a -> int) f m1 m2 =
    let rec common m1 m2 =
      match (m1, m2) with
      | (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, x1, d1, r1, _), t2) ->
	  match split compare x1 t2 with
	  | (l2, None, r2) ->
	      concat compare (common l1 l2) (common r1 r2)
	  | (l2, (Some d2), r2) ->
	      join compare (common l1 l2) x1 (f x1 d1 d2) (common r1 r2)
    in
    common m1 m2

  let interset (compare:'a -> 'a -> int) m1 s2 =
    let rec interset m1 s2 =
      match (m1, s2) with
      | (Empty, t2) -> Empty
      | (t1, Sette.Empty) -> Empty
      | (Node(l1, x1, d1, r1, _), t2) ->
	  match Sette.Compare.split compare x1 t2 with
	  | (l2, false, r2) ->
	      concat compare (interset l1 l2) (interset r1 r2)
	  | (l2, true, r2) ->
	      join compare (interset l1 l2) x1 d1 (interset r1 r2)
    in
    interset m1 s2

  let diffset (compare:'a -> 'a -> int) m1 s2 =
    let rec diffset m1 s2 =
      match (m1, s2) with
      (Empty, t2) -> Empty
      | (t1, Sette.Empty) -> t1
      | (Node(l1, x1, d1, r1, _), t2) ->
	  match Sette.Compare.split compare x1 t2 with
	  (l2, false, r2) ->
	    join compare (diffset l1 l2) x1 d1 (diffset r1 r2)
	  | (l2, true, r2) ->
	      concat compare (diffset l1 l2) (diffset r1 r2)
    in
    diffset m1 s2

  let rec filter (compare:'a -> 'a -> int) p = function
    | Empty -> Empty
    | Node(l, v, d, r, _) ->
	(* call [p] in the expected left-to-right order *)
	let l' = filter compare p l in
	let pvd = p v d in
	let r' = filter compare p r in
	if pvd then join compare l' v d r' else concat compare l' r'

  let rec partition (compare:'a -> 'a -> int) p = function
    | Empty -> (Empty, Empty)
    | Node(l, v, d, r, _) ->
	(* call [p] in the expected left-to-right order *)
	let (lt, lf) = partition compare p l in
	let pvd = p v d in
	let (rt, rf) = partition compare p r in
	if pvd
	then (join compare lt v d rt, concat compare lf rf)
	else (concat compare lt rt, join compare lf v d rf)

  type ('a,'b) enumeration = End | More of 'a * 'b * ('a,'b) t * ('a,'b) enumeration

  let rec cons_enum m e =
    match m with
    | Empty -> e
    | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

  let compare (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	  let c = compare v1 v2 in
	  if c <> 0 then c else
	    let c = cmp d1 d2 in
	    if c <> 0 then c else
	      compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    if m1==(Obj.magic m2) then 0 else compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let comparei (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match (e1, e2) with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	  let c = compare v1 v2 in
	  if c <> 0 then c else
	    let c = cmp v1 d1 d2 in
	    if c <> 0 then c else
	      compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    if m1==(Obj.magic m2) then 0 else compare_aux (cons_enum m1 End) (cons_enum m2 End)

  let equal (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
      (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	  compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    m1==(Obj.magic m2) || equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let equali (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
      (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
	  compare v1 v2 = 0 && cmp v1 d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
    in
    m1==(Obj.magic m2) || equal_aux (cons_enum m1 End) (cons_enum m2 End)

  let subset (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec subset_aux m1 m2 =
      match (m1, m2) with
      | Empty, _ ->
	  true
      | _, Empty ->
	  false
      | Node (l1, v1, d1, r1, _), (Node (l2, v2, d2, r2, _) as t2) ->
	  let c = compare v1 v2 in
	  if c = 0 then
	    cmp d1 d2 && subset_aux l1 l2 && subset_aux r1 r2
	  else if c < 0 then
	    subset_aux (Node (l1, v1, d1, Empty, 0)) l2 && subset_aux r1 t2
	  else
	    subset_aux (Node (Empty, v1, d1, r1, 0)) r2 && subset_aux l1 t2
    in
    m1==(Obj.magic m2) || subset_aux m1 m2

  let subseti (compare:'a -> 'a -> int) cmp m1 m2 =
    let rec subset_aux m1 m2 =
      match (m1, m2) with
      | Empty, _ ->
	  true
      | _, Empty ->
	  false
      | Node (l1, v1, d1, r1, _), (Node (l2, v2, d2, r2, _) as t2) ->
	  let c = compare v1 v2 in
	  if c = 0 then
	    cmp v1 d1 d2 && subset_aux l1 l2 && subset_aux r1 r2
	  else if c < 0 then
	    subset_aux (Node (l1, v1, d1, Empty, 0)) l2 && subset_aux r1 t2
	  else
	    subset_aux (Node (Empty, v1, d1, r1, 0)) r2 && subset_aux l1 t2
    in
    m1==(Obj.magic m2) || subset_aux m1 m2

  let rec merge (compare:'a -> 'a -> int) dmerge m1 m2 =
    match (m1, m2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, x1, d1, r1, h1), Node(l2, x2, d2, r2, h2)) ->
	if h1 >= h2 then begin
	  let (l2, pres, r2) = split compare x1 m2 in
	  join
	    compare
	    (merge compare dmerge l1 l2)
	    x1
	    (match pres with None -> d1 | Some d2 -> dmerge d1 d2)
	    (merge compare dmerge r1 r2)
	end
	else begin
	  let (l1, pres, r1) = split compare x2 m1 in
	  join
	    compare
	    (merge compare dmerge l1 l2)
	    x2
	    (match pres with None -> d2 | Some d1 -> dmerge d1 d2)
	    (merge compare dmerge r1 r2)
	end

  let rec mergei (compare:'a -> 'a -> int) dmerge m1 m2 =
    match (m1, m2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, x1, d1, r1, h1), Node(l2, x2, d2, r2, h2)) ->
	if h1 >= h2 then begin
	  let (l2, pres, r2) = split compare x1 m2 in
	  join
	    compare
	    (mergei compare dmerge l1 l2)
	    x1
	    (match pres with None -> d1 | Some d2 -> dmerge x1 d1 d2)
	    (mergei compare dmerge r1 r2)
	end
	else begin
	  let (l1, pres, r1) = split compare x2 m1 in
	  join
	    compare
	    (mergei compare dmerge l1 l2)
	    x2
	    (match pres with None -> d2 | Some d1 -> dmerge x1 d1 d2)
	    (mergei compare dmerge r1 r2)
	end

  let rec combine
    (compare : 'a -> 'a -> int)
    (dcombine: 'a -> 'b option -> 'c option -> 'd option)
    (m1:('a,'b) t)
    (m2:('a,'c) t)
    :
    ('a,'d) t
    =
    match (m1, m2) with
    | (Empty, t2) ->
	fold
	(fun k d res ->
	  match dcombine k None (Some d) with
	  | None -> res
	  | Some d -> add compare k d res
	)
	t2 empty
    | (t1, Empty) ->
	fold
	(fun k d res ->
	  match dcombine k (Some d) None with
	  | None -> res
	  | Some d -> add compare k d res
	)
	t1 empty
    | (Node(l1, x1, d1, r1, h1), Node(l2, x2, d2, r2, h2)) ->
	if h1 >= h2 then begin
	  let (l2, pres, r2) = split compare x1 m2 in
	  let data = dcombine x1 (Some d1) pres in
	  match data with
	  | None ->
	      concat compare (combine compare dcombine l1 l2) (combine compare dcombine r1 r2)
	  | Some d ->
	      join
	      compare
	      (combine compare dcombine l1 l2)
	      x1 d
	      (combine compare dcombine r1 r2)
	end
	else begin
	  let (l1, pres, r1) = split compare x2 m1 in
	  let data = dcombine x2 pres (Some d2)in
	  match data with
	  | None ->
	      concat compare (combine compare dcombine l1 l2) (combine compare dcombine r1 r2)
	  | Some d ->
	      join
	      compare
	      (combine compare dcombine l1 l2)
	      x2 d
	      (combine compare dcombine r1 r2)
	end
end

let add x data map = Compare.add Pervasives.compare x data map
let find x map = Compare.find Pervasives.compare x map
let mem x map = Compare.mem Pervasives.compare x map
let remove x map = Compare.remove Pervasives.compare x map
let interset m1 s2 = Compare.interset Pervasives.compare m1 s2
let diffset m1 s2 = Compare.diffset Pervasives.compare m1 s2
let filter f m = Compare.filter Pervasives.compare f m
let partition f m = Compare.partition Pervasives.compare f m
let compare cmp m1 m2 = Compare.compare Pervasives.compare cmp m1 m2
let comparei cmp m1 m2 = Compare.comparei Pervasives.compare cmp m1 m2
let equal cmp m1 m2 = Compare.equal Pervasives.compare cmp m1 m2
let equali cmp m1 m2 = Compare.equali Pervasives.compare cmp m1 m2
let subset cmp m1 m2 = Compare.subset Pervasives.compare cmp m1 m2
let subseti cmp m1 m2 = Compare.subseti Pervasives.compare cmp m1 m2
let common f m1 m2 = Compare.common Pervasives.compare f m1 m2
let commoni f m1 m2 = Compare.commoni Pervasives.compare f m1 m2
let addmap m1 m2 = Compare.addmap Pervasives.compare m1 m2
let merge dmerge m1 m2 = Compare.merge Pervasives.compare dmerge m1 m2
let mergei dmerge m1 m2 = Compare.mergei Pervasives.compare dmerge m1 m2
let combine dcombine m1 m2 = Compare.combine Pervasives.compare dcombine m1 m2

(** Output signature of the functor {!Mappe.Make}. *)
module type S = sig
  type key
  type 'a t
  module Setkey : (Sette.S with type elt=key)

  val repr : 'a t -> (key,'a) map
  val obj : (key,'a) map -> 'a t

  val is_empty : 'a t -> bool
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val addmap : 'a t -> 'a t -> 'a t
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val mergei : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val common : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val commoni : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val combine : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val interset : 'a t -> Setkey.t -> 'a t
  val diffset : 'a t -> Setkey.t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val maptoset : 'a t -> Setkey.t
  val mapofset: (key -> 'a) -> Setkey.t -> 'a t
  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  val comparei : (key -> 'a -> 'b -> int) -> 'a t -> 'b t -> int
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val equali : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val subset : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val subseti : (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_key: 'a t -> key
  val max_key: 'a t -> key
  val choose : 'a t -> key * 'a
  val print :
    ?first:(unit, Format.formatter, unit) format ->
    ?sep:(unit, Format.formatter, unit) format ->
    ?last:(unit, Format.formatter, unit) format ->
    ?firstbind:(unit, Format.formatter, unit) format ->
    ?sepbind:(unit, Format.formatter, unit) format ->
    ?lastbind:(unit, Format.formatter, unit) format ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
end

(** Functor building an implementation of the map structure
   given a totally ordered type. *)
module Make(Setkey : Sette.S) = struct
  type key = Setkey.elt
  module Setkey=Setkey

  type 'a t = (key,'a) map

  let repr : 'a t -> (key,'a) map = Obj.magic
  let obj : (key,'a) map -> 'a t = Obj.magic

  let is_empty (t:'a t) = t==Empty
  let empty : 'a t = empty
  let iter = iter
  let map = map
  let mapi = mapi
  let fold = fold
  let maptoset x = Setkey.obj (maptoset (repr x))
  let mapofset f x = mapofset f (Setkey.repr x)
  let cardinal = cardinal
  let bindings = bindings
  let min_key = min_key
  let max_key = max_key
  let choose = choose

  let add x data map = Compare.add Setkey.Ord.compare x data map
  let find x map = Compare.find Setkey.Ord.compare x map
  let mem x map = Compare.mem Setkey.Ord.compare x map
  let remove x map = Compare.remove Setkey.Ord.compare x map
  let interset m1 s2 = Compare.interset Setkey.Ord.compare m1 (Setkey.repr s2)
  let diffset m1 s2 = Compare.diffset Setkey.Ord.compare m1 (Setkey.repr s2)
  let filter f m = Compare.filter Setkey.Ord.compare f m
  let partition f m = Compare.partition Setkey.Ord.compare f m
  let compare cmp m1 m2 = Compare.compare Setkey.Ord.compare cmp m1 m2
  let comparei cmp m1 m2 = Compare.comparei Setkey.Ord.compare cmp m1 m2
  let equal cmp m1 m2 = Compare.equal Setkey.Ord.compare cmp m1 m2
  let equali cmp m1 m2 = Compare.equali Setkey.Ord.compare cmp m1 m2
  let subset cmp m1 m2 = Compare.subset Setkey.Ord.compare cmp m1 m2
  let subseti cmp m1 m2 = Compare.subseti Setkey.Ord.compare cmp m1 m2
  let common f m1 m2 = Compare.common Setkey.Ord.compare f m1 m2
  let commoni f m1 m2 = Compare.commoni Setkey.Ord.compare f m1 m2
  let addmap m1 m2 = Compare.addmap Setkey.Ord.compare m1 m2
  let merge dmerge m1 m2 = Compare.merge Setkey.Ord.compare dmerge m1 m2
  let mergei dmerge m1 m2 = Compare.mergei Setkey.Ord.compare dmerge m1 m2
  let combine dcombine m1 m2 = Compare.combine Setkey.Ord.compare dcombine m1 m2
  let print = print
end
