(* Sample code querying module
 * parses programs from the standard input
 *)

open Types
open CS_Interop_Types

type blkdesc = csblock
type funcdesc = csblock array csfunc

let file = ref None

let getfg () =
	match !file with
  	| None -> failwith "Cquery: module not initialized"
  	| Some x -> x

let getg () = 
	getfg () |> fst
let getf () = 
	getfg () |> snd

let init dump_blk filename =
  (* lists of global vars and functions *)
  let globals, cfg_funcs = CS_Interop_Lexer.cfg_file stdin in
  let globals, cfg_funcs = CS_Utils.transform_callargs dump_blk globals cfg_funcs in 
  file := Some (globals, cfg_funcs)

let get_glos () = 
  getg ()

let get_main () =
  let fdefs = getf () in
  List.find (fun x -> x.fname = "start") fdefs

let get_func fn =
  let fdefs = getf () in
  List.find (fun x -> x.fname = fn) fdefs

let get_name f = 
  f.fname

let get_args f = 
  f.fargs 

let get_locs f = 
  f.flocs

let get_nblk f = 
  Array.length f.fbody

let get_blk f n = 
  f.fbody.(n)

let get_nins b = 
  List.length b.binsts

let get_ins b n =
  List.nth b.binsts n

let get_jmp b = 
  b.bjump

let get_preds b = 
  b.bpreds

