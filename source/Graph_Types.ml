(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types

type node = int

type action =
  | ANone
  | AWeaken
  | AGuard of logic
  | AAssign of id * expr ref * bool ref (* potential-carrying assignment *)
  | AAddMemReads of id * id * int option ref * int * bool * bool
  (* tick var, input var, placeholder for cost, bits, host, read *)
  | AAddConflicts of id * id * int option ref * int * bool
  (* tick var, input var, placeholder for cost, bits, read *)
  | ACall of id
  | AProb of float (* probabilistic branching *)
  | AUnique of logic ref
  | ANotUnique of logic ref
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
