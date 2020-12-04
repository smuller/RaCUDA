(** Sets over ordered types, parametrized polymorphic version *)

type 'a t = {
  compare : 'a -> 'a -> int;
  set : 'a Sette.t;
}
let make compare set = { compare=compare; set=set }

let empty compare = make compare Sette.empty
let is_empty t = Sette.is_empty t.set
let singleton compare x = make compare (Sette.singleton x)
let iter f t = Sette.iter f t.set
let fold f t acc = Sette.fold f t.set acc
let for_all f t = Sette.for_all f t.set
let exists f t = Sette.exists f t.set
let cardinal t = Sette.cardinal t.set
let elements t = Sette.elements t.set
let min_elt t = Sette.min_elt t.set
let max_elt t = Sette.max_elt t.set
let choose = min_elt
let print ?first ?sep ?last px fmt t = Sette.print ?first ?sep ?last px fmt t.set
let add x t = make t.compare (Sette.Compare.add t.compare x t.set)
let mem x t = Sette.Compare.mem t.compare x t.set
let remove x t = make t.compare (Sette.Compare.remove t.compare x t.set)
let union t1 t2 = 
  make t1.compare (Sette.Compare.union t1.compare t1.set t2.set)
let inter t1 t2 = 
  make t1.compare (Sette.Compare.inter t1.compare t1.set t2.set)
let diff t1 t2 = 
  make t1.compare (Sette.Compare.diff t1.compare t1.set t2.set)
let equal t1 t2 =
  Sette.Compare.equal t1.compare t1.set t2.set
let compare t1 t2 = 
  Sette.Compare.compare t1.compare t1.set t2.set
let subset t1 t2 =
  Sette.Compare.subset t1.compare t1.set t2.set
let filter f t = 
  make t.compare (Sette.Compare.filter t.compare f t.set)
let partition f t = 
  let (set1,set2) = Sette.Compare.partition t.compare f t.set in
  (make t.compare set1, make t.compare set2)
