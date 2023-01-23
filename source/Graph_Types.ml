(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types

type node = int

type annot = (unit CUDA_Types.cexpr * unit CUDA_Types.cexpr) option ref
type uexpr = unit expr
type ulogic = unit logic
          
type action =
  | ANone
  | AWeaken
  | AGuard of ulogic
  | AAssign of id * annot expr ref * bool ref (* potential-carrying assignment *)
  | AAddMemReads of id * id * annot * int option ref * int * bool * bool
  (* tick var, input var, input annot, placeholder for cost, bits, host, read *)
  | AAddConflicts of id * id * annot * int option ref * int * bool
  (* tick var, input var, input annot, placeholder for cost, bits, read *)
  | ACall of id
  | AProb of float (* probabilistic branching *)
  | AUnique of ulogic ref
  | ANotUnique of ulogic ref
  | ACallUninterp of id * string * id list
  | AUnify
  | ASwitch of id list

type graph =
  { g_start: node
  ; g_end: node
  ; g_edges: ((action * node) list) array
  ; g_position: position array
  }

type func = (Focus.focus, graph) func_
