(* Bertrand Jeannet. This file is released under LGPL license. *)

(** Imbricated lists *)

type ('a,'b) el =
    Atome of 'b
  | List of ('a,'b) t
and ('a,'b) t = 'a * ('a,'b) el list

let cons x l = let (b,ll) = l in (b,x::ll)
let atome x = Atome(x)
let list b l = List(b,l)

let hd l = let (_,ll) = l in List.hd ll
let tl l = let (b,ll) = l in (b,List.tl ll)
let length l = let (_,ll) = l in List.length ll

let depth (b,ll) =
  let rec depth maxdepth ll = match ll with
    | [] -> maxdepth
    | (Atome _)::ll -> depth maxdepth ll
    | (List(_,ll2))::ll ->
	depth (max maxdepth (1+(depth 1 ll2))) ll
  in
  depth 1 ll
    
let append ~combine (b1,ll1) (b2,ll2) =
  (combine b1 b2, ll1 @ ll2)

let rev_append
    ~(combine:('b -> 'b -> 'b))
    (b1,ll1) (b2,ll2)
    =
  let rec rev_append nb ll1 ll2 =
    match ll1 with
    | [] -> (nb,ll2)
    | (Atome(a) as x1)::ll1 ->
	rev_append nb ll1 (x1::ll2)
    | List(b,ll)::ll1 ->
	rev_append nb ll1 (List(rev_append b ll [])::ll2)
  in
  let nb = combine b1 b2 in
  rev_append nb ll1 ll2

let rev ((b,ll) as l) = rev_append ~combine:(fun _ _ -> b) l (b,[])

let exists p ilist =
  let rec exists b = function
  | [] -> false
  | Atome(a)::ll -> p b a || exists b ll
  | List(b2,ll2)::ll -> exists b2 ll2 || exists b ll
  in
  let (b,ll) = ilist in
  exists b ll

let mem x l = exists (fun _ a -> x=a) l

let map fb fa ilist =
  let rec parcours flag b = function
    | [] -> []
    | Atome(a)::ll ->
        Atome(fa flag b a)::(parcours false b ll)
    | List(b2,ll2)::ll ->
	let b3 = fb b2 in
	let ll3 = parcours true b2 ll2 in
	List(b3,ll3)::(parcours false b ll)
  in
  let (b,ll) = ilist in
  (fb b, (parcours false b ll))

let iter f ilist =
  let rec parcours flag b = function
    | [] -> ()
    | Atome(a)::ll ->
	f flag b a;
	parcours false b ll
    | List(b2,ll2)::ll ->
	parcours true b2 ll2;
	parcours false b ll
  in
  let (b,l) = ilist in
  parcours false b l

let fold_left f res ilist =
  let rec parcours res flag b = function
    | [] -> res
    | Atome(a)::ll ->
	let nres = f res flag b a in
	parcours nres false b ll
    | List(b2,ll2)::ll ->
	let nres = parcours res true b2 ll2 in
	parcours nres false b ll
  in
  let (b,l) = ilist in
  parcours res false b l

let fold_right f ilist res =
  let rec parcours res flag b = function
    | [] -> res
    | Atome(a)::ll ->
	let nres = parcours res false b ll in
	f flag b a nres
    | List(b2,ll2)::ll ->
	let nres = parcours res false b ll in
	parcours nres true b2 ll2
  in
  let (b,l) = ilist in
  parcours res false b l

let of_list b list =
  let rilist =
    List.fold_left
      (fun res a -> Atome(a)::res)
      [] list
  in
  (b, List.rev rilist)

let to_list ilist =
  let list =
    fold_left
      (fun res flag b elt -> elt::res)
      [] ilist
  in
  List.rev list

let flatten ?(depth=1) ilist
  =
  let rec rev_flatten (res:('a,'b) el list) (cdepth:int) = function
    | [] -> res
    | x::l ->
	let nres = begin match x with
	| Atome(_) -> x::res
	| List(b,ll) ->
	    if cdepth < depth then
	      let ll2 = rev_flatten [] (cdepth+1) ll in
	      (List(b,ll2))::res
	    else
	      fold_left
		(fun res flag b a -> Atome(a)::res)
		res (b,ll)
	end
	in
	rev_flatten nres cdepth l
  in
  let (b,ll) = ilist in
  let res = rev_flatten [] 1 ll in
  rev (b,res)

let print
  ?first ?sep ?last
  ?(firstexp : (unit, Format.formatter, unit) format = ("^":(unit, Format.formatter, unit) format))
  ?(lastexp : (unit, Format.formatter, unit) format = ("":(unit, Format.formatter, unit) format))
  print_exp print_atom fmt ilist
  =
  let rec print_elt fmt ll = match ll with
    | Atome(a) -> print_atom fmt a
    | List(b,ll) ->
	Print.list ?first ?sep ?last print_elt fmt ll;
	Format.fprintf fmt firstexp;
	print_exp fmt b;
	Format.fprintf fmt lastexp
  in
  print_elt fmt (List ilist)
