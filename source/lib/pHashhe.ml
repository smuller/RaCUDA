(** Hash tables, parametrized polymorphic version *)

type 'a compare = 'a Hashhe.compare = {
  hash : 'a -> int;
  equal : 'a -> 'a -> bool;
}

type ('a, 'b) t = {
  compare : 'a Hashhe.compare;
  mutable hashtbl : ('a, 'b) Hashhe.t
}

let stdcompare = Hashhe.stdcompare

let make compare hashtbl = { compare=compare; hashtbl=hashtbl }
let create_compare compare n = make compare (Hashhe.create n)
let create hash equal n = 
  let compare = {
    hash = (fun key -> (hash key) land max_int);
    equal = equal
  }
  in
  make compare (Hashhe.create n)
    
let clear t = Hashhe.clear t.hashtbl
let copy t = make t.compare (Hashhe.copy t.hashtbl)
let map f t = make t.compare (Hashhe.map f t.hashtbl)
let iter f t = Hashhe.iter f t.hashtbl
let fold f t a = Hashhe.fold f t.hashtbl a
let length t = Hashhe.length t.hashtbl
let print ?first ?sep ?last ?firstbind ?sepbind ?lastbind pa pb fmt t = 
  Hashhe.print ?first ?sep ?last ?firstbind ?sepbind ?lastbind pa pb fmt t.hashtbl
    
let add t key info = Hashhe.Compare.add t.compare t.hashtbl key info
let replace t key info = Hashhe.Compare.replace t.compare t.hashtbl key info
let remove t key = Hashhe.Compare.remove t.compare t.hashtbl key 
let find t key = Hashhe.Compare.find t.compare t.hashtbl key 
let find_all t key = Hashhe.Compare.find_all t.compare t.hashtbl key 
let mem t key = Hashhe.Compare.mem t.compare t.hashtbl key 

