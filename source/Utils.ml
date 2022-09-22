(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types

exception Error
exception Todo of string

let _TODO s = raise (Todo s)

let dummy_position =
  { pos_file = ""
  ; pos_line = -1
  ; pos_bol = -1
  ; pos_char = -1 }

let print_position fmt pos =
  Format.fprintf fmt "%s:%i:%i"
    pos.pos_file pos.pos_line
    (pos.pos_char - pos.pos_bol + 1)

let rec expr_to_str e = 
  match desc e with
  | ERandom -> 
    Printf.sprintf "random"
  | EVar id -> 
    Printf.sprintf "%s" id
  | ENum i ->
    Printf.sprintf "%d" i
  | EAdd (e1, e2) -> 
    (expr_to_str e1) ^ " + " ^ (expr_to_str e2)
  | ESub (e1, e2) -> 
    (expr_to_str e1) ^ " - " ^ (expr_to_str e2)
  | EMul (e1, e2) -> 
    (expr_to_str e1) ^ " * " ^ (expr_to_str e2)
  | EBer (a, b) -> 
    Printf.sprintf "ber(%d, %d)" a b
  | EBin (n, a, b) ->
    Printf.sprintf "bin(%d, %d, %d)" n a b  
  | EGeo (a, b) ->
    Printf.sprintf "geo(%d, %d)" a b
  | ENbin (r, a, b) ->
    Printf.sprintf "nbin(%d, %d, %d)" r a b
  | EPois (a, b) ->
    Printf.sprintf "pois(%d, %d)" a b
  | EHyper (n, r, m) ->
    Printf.sprintf "hyper(%d, %d, %d)" n r m
  | EUnif (a, b) ->
    Printf.sprintf "unif(%d, %d)" a b
  | EDist (name, a, b, c, d) ->
    Printf.sprintf "%s(%d, %d, %d, %d)" name a b c d

let pexpr_to_str pe = 
  match pe with
  | EProb (a, b) ->
    Printf.sprintf "prob(%d, %d)" a b

let rec fexpr_to_str fe =
  match fe with
  | FBase e ->
    expr_to_str e
  | FApply (id, fe_l, pos) ->
    let fe_l_str = List.fold_left (fun init e -> fexpr_to_str e ^ ";" ^ init) "" fe_l in 
    (Printf.sprintf "at %s: " id) ^ fe_l_str

let cmp_to_str cmp =
  match cmp with
    Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"
  | Eq -> "="
  | Ne -> "!="

let rec logic_to_str lg = 
  match lg with
    LTrue -> 
    "true"
  | LFalse -> 
    "false"
  | LRandom ->
    "random"
  | LCmp (e1, c, e2) ->
    (expr_to_str e1) ^ " " ^ (cmp_to_str c) ^ " " ^ (expr_to_str e2)
  | LAnd (lg1, lg2) ->
    (logic_to_str lg1) ^ " and " ^ (logic_to_str lg2)
  | LOr (lg1, lg2) ->
    (logic_to_str lg1) ^ " or " ^ (logic_to_str lg2)
  | LNot lg1 ->
    "not " ^ (logic_to_str lg1)

let negate_cmp = function
  | Le -> Gt
  | Lt -> Ge
  | Ge -> Lt
  | Gt -> Le
  | Eq -> Ne
  | Ne -> Eq

let fresh_name =
  let cnt = ref 0 in
  fun () ->
    let n = "fresh." ^ string_of_int !cnt in
    incr cnt;
    n

let is_fresh n =
  String.length n >= 6 &&
  String.sub n 0 6 = "fresh."

let show_remove_png fpng =
  let viewers = [ "feh"; "display"; "open" ] in
  match Unix.fork () with
  | 0 ->
    List.iter begin fun prog ->
      let cmd = Printf.sprintf "%s %s >/dev/null 2>&1" prog fpng in
      if (try Sys.command cmd with Sys_error _ -> 1) = 0
      then (Sys.remove fpng; exit 0)
    end viewers;
    Printf.eprintf "Could open file '%s' for display.\n" fpng;
    exit 0
  | _ -> ()

let exit n = exit n

let add_tick_var tick_var globals =
  if (List.mem tick_var globals) then
    globals
  else tick_var :: globals

