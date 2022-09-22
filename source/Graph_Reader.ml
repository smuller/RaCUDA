(* Quentin Carbonneaux - 2016-2017 *)

open Types
open Graph_Types

let error e =
  Format.eprintf "graph reader error: %s@." e;
  raise Utils.Error

let read_byte ic =
  Char.code (input_char ic)

let read_int ic =
  let n =
  let rec go n acc =
    if n = 0 then acc else
    go (n-1) ((acc lsl 8) + read_byte ic)
  in go 8 0
  in
  if abs n > 100000 then failwith (Printf.sprintf "oops, suspicious %x" n) else n

let read_string ic =
  let size = read_int ic in
  let buf = Bytes.create size in
  for i = 0 to size-1 do
    Bytes.set buf i (input_char ic)
  done;
  Bytes.to_string buf

let rec read_expr ic =
  let binop f =
    let l = read_expr ic in
    let r = read_expr ic in
    f l r
  in
  match read_byte ic with
  | 1 -> binop (fun l r -> mk () (EAdd (l, r)))
  | 2 -> binop (fun l r -> mk () (ESub (l, r)))
  | 3 -> binop (fun l r -> mk () (EMul (l, r)))
  | 4 -> mk () (ENum (read_int ic))
  | 5 -> mk () (EVar (read_string ic))
  | 6 -> mk () ERandom
  | _ -> error "invalid expression type"

let read_logic ic =
  match read_byte ic with
  | 1 -> LTrue
  | 2 -> LFalse
  | 3 -> LRandom
  | (4 | 5 | 6 | 7 | 8 | 9) as b ->
    let cmp = [| Le; Ge; Lt; Gt; Eq; Ne |] in
    let l = read_expr ic in
    let r = read_expr ic in
    LCmp (l, cmp.(b-4), r)
  | _ -> error "invalid logic type"

let read_edge ic =
  let action =
    match read_byte ic with
    | 1 -> AGuard (read_logic ic)
    | 2 ->
      let v = read_string ic in
      let e = read_expr ic in
      let pot = (match read_byte ic with
                 | 0 -> false
                 | 1 -> true
                 | _ -> error "invalid edge type")
      in
      AAssign (v, ref e, ref pot)
    | 3 -> ANone
    | _ -> error "invalid edge type"
  in (action, read_int ic)

let read_list read ic =
  let size = read_int ic in
  let rec go n =
    if n = 0 then [] else
    let x = read ic in
    x :: go (n-1)
  in go size

let read_func ic =
  let name = read_string ic in
  let args = read_list read_string ic in
  let locs = read_list read_string ic in
  let start = read_int ic in
  let ret = read_int ic in
  let edges = read_list (read_list read_edge) ic in
  let edges = Array.of_list edges in
  { fun_name = name
  ; fun_vars = locs @ args
  ; fun_focus = []
  ; fun_body =
    { g_start = start
    ; g_end = ret
    ; g_edges = edges
    ; g_position =
      Array.map (fun _ -> Utils.dummy_position) edges
    }
  ; fun_start_p = Utils.dummy_position
  ; fun_end_p = Utils.dummy_position
  ; fun_args = []
  }
