(** Association tables over ordered types, parametrized polymorphic version *)

type ('a,'b) t = {
  compare : ('a -> 'a -> int);
  map : ('a,'b) Mappe.t
}
let make compare map = {compare=compare;map=map}
  
let is_empty (t:('a,'b) t) = t.map==Mappe.Empty
let empty compare = make compare Mappe.empty
let iter f t = Mappe.iter f t.map
let map f t = make t.compare (Mappe.map f t.map)
let mapi f t = make t.compare (Mappe.mapi f t.map)
let fold f t acc = Mappe.fold f t.map acc
let maptoset t = PSette.make t.compare (Mappe.maptoset t.map)
let mapofset f t = make t.PSette.compare (Mappe.mapofset f t.PSette.set)
let cardinal t = Mappe.cardinal t.map
let bindings t = Mappe.bindings t.map
let min_key t = Mappe.min_key t.map
let max_key t = Mappe.max_key t.map
let choose t = Mappe.choose t.map
  
let add x data t = make t.compare (Mappe.Compare.add t.compare x data t.map)
let find x t = Mappe.Compare.find t.compare x t.map
let mem x t = Mappe.Compare.mem t.compare x t.map
let remove x t = make t.compare (Mappe.Compare.remove t.compare x t.map)
let interset m1 s2 =
  assert(m1.compare==s2.PSette.compare);
  make m1.compare
    (Mappe.Compare.interset m1.compare m1.map s2.PSette.set)
let diffset m1 s2 =
  assert(m1.compare==s2.PSette.compare);
  make m1.compare
    (Mappe.Compare.diffset m1.compare m1.map s2.PSette.set)
let filter f m = make m.compare (Mappe.Compare.filter m.compare f m.map)
let partition f m =
  let (m1,m2) = Mappe.Compare.partition m.compare f m.map in
  (make m.compare m1, make m.compare m2)
    
let mapfm12b foo cmp m1 m2 =
  foo m1.compare cmp m1.map m2.map
let compare cmp m1 m2 = mapfm12b Mappe.Compare.compare cmp m1 m2
let comparei cmp m1 m2 = mapfm12b Mappe.Compare.comparei cmp m1 m2
let equal cmp m1 m2 = mapfm12b Mappe.Compare.equal cmp m1 m2
let equali cmp m1 m2 = mapfm12b Mappe.Compare.equali cmp m1 m2
let subset cmp m1 m2 = mapfm12b Mappe.Compare.subset cmp m1 m2
let subseti cmp m1 m2 = mapfm12b Mappe.Compare.subseti cmp m1 m2
  
let mapfm12m foo f m1 m2 =
  assert(m1.compare==m2.compare);
  make m1.compare (foo m1.compare f m1.map m2.map)
    
let common f m1 m2 = mapfm12m Mappe.Compare.common f m1 m2
let commoni f m1 m2 = mapfm12m Mappe.Compare.commoni f m1 m2
let addmap m1 m2 = 
  assert(m1.compare==m2.compare);
  make m1.compare (Mappe.Compare.addmap m1.compare m1.map m2.map)
let merge f m1 m2 = mapfm12m Mappe.Compare.merge f m1 m2
let mergei f m1 m2 = mapfm12m Mappe.Compare.mergei f m1 m2
let combine f m1 m2 = mapfm12m Mappe.Compare.combine f m1 m2
  
let print ?first ?sep ?last ?firstbind ?sepbind ?lastbind px py fmt m =
  Mappe.print ?first ?sep ?last ?firstbind ?sepbind ?lastbind px py fmt m.map


