(** Small module to compute the duration of computations *)

let wrap_duration (duration:float ref) (f:unit -> 'a) : 'a =
  let t1 = Sys.time() in
  try 
    let res = f() in
    let t2 = Sys.time() in
    duration := t2 -. t1;
    res
  with exn ->
    let t2 = Sys.time() in
    duration := t2 -. t1;
    raise exn
    

let wrap_duration_add (duration:float ref) (f:unit -> 'a) : 'a =
  let t1 = Sys.time() in
  try 
    let res = f() in
    let t2 = Sys.time() in
    duration := !duration +. (t2 -. t1);
    res
  with exn ->
    let t2 = Sys.time() in
    duration := !duration +. (t2 -. t1);
    raise exn
