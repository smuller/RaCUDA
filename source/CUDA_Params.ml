open Types
open CUDA_Types
open CUDA
open CUDA_Cost

module M = Map.Make(Id)

let const_array warp c =
   Array.make warp (Some c)

let allocarr f warp id l (p, m, h, i) =
  let l' = List.map f l in
  let a = Array.of_list l' in
  let (ptr, h') = Heap.alloc h a in
  (p, M.add id (const_array warp ptr) m, h', i + 1)

let alloccarr x warp id (p, m, h, i) =
  let (ptr, h') = Heap.calloc h x in
  (p, M.add id (const_array warp ptr) m, h', i + 1)

let params_of_line (p, m, h, i) s =
  let dim3_of_xyz (x, y, z) =
    {x = int_of_string x;
     y = int_of_string y;
     z = int_of_string z}
  in
  match String.split_on_char ' ' s with
  | [] -> (p, m, h, i + 1)
  | ["gridDim"; x; y; z] ->
     ({p with griddim = dim3_of_xyz (x, y, z)}, m, h, i + 1)
  | ["blockDim"; x; y; z] ->
     ({p with blockdim = dim3_of_xyz (x, y, z)}, m, h, i + 1)
  | ["blockIdx"; x; y; z] ->
     ({p with blockidx = Some (dim3_of_xyz (x, y, z))}, m, h, i + 1)
  | ["initThread"; x; y; z] ->
     ({p with init_thread = Some (dim3_of_xyz (x, y, z))}, m, h, i + 1)
  | ["warpSize"; s] ->
     ({p with warp_size = int_of_string s}, m, h, i + 1)
  | ["globalReadSize"; s] ->
     ({p with global_read_size = int_of_string s}, m, h, i + 1)
  | ["hostReadSize"; s] ->
     ({p with host_read_size = int_of_string s}, m, h, i + 1)
  | ["numSharedBanks"; n] ->
     ({p with num_shared_banks = int_of_string s}, m, h, i + 1)
  | ["valOfInt"; id; v] ->
     (p, M.add id (const_array (p.warp_size) (CInt (int_of_string v))) m, h,
      i + 1)
  | ["valOfFloat"; id; v] ->
     (p, M.add id (const_array (p.warp_size) (CFloat (float_of_string v))) m, h,
      i + 1)
  | ["valOfString"; id; v] ->
     (p, M.add id (const_array (p.warp_size) (CString v)) m, h, i + 1)
  | ["valOfChar"; id; v] ->
     (p, M.add id (const_array (p.warp_size) (CChar (v.[0]))) m, h, i + 1)
  | "arrInt"::id::l ->
     allocarr (fun v -> CInt (int_of_string v)) (p.warp_size) id l (p, m, h, i)
  | "arrFloat"::id::l ->
     allocarr (fun v -> CFloat (float_of_string v)) (p.warp_size) id l
       (p, m, h, i)
  | "arrString"::id::l ->
     allocarr (fun v -> CString v) (p.warp_size) id l (p, m, h, i)
  | "arrChar"::id::l ->
     allocarr (fun v -> CChar (v.[0])) (p.warp_size) id l (p, m, h, i)
  | ["arrConstFloat"; id; x] ->
     alloccarr (CFloat (float_of_string x)) (p.warp_size) id (p, m, h, i)
  | s::_ when s.[0] = '#' -> (p, m, h, i + 1)
  | _ -> (Printf.printf "Warning: invalid parameter on line %d\n" i;
          (p, m, h, i + 1))


let default_params =
  { griddim = {x = 1; y = 1; z = 1};
    blockdim = {x = 32; y = 1; z = 1};
    blockidx = None; (* Some {x = 0; y = 0; z = 0}; *)
    global_read_size = 256;
    host_read_size = 256;
    num_shared_banks = 32;
    warp_size = 32;
    sizeof = sizeof64;
    init_thread = None}

let default_map = M.empty

let rec read (p, m, h, i) c =
  try
    read (params_of_line (p, m, h, i) (input_line c)) c
  with End_of_file -> (p, m, h)

let params_of_file file =
  let c = open_in file in
  read (default_params, default_map, Heap.empty, 1) c
