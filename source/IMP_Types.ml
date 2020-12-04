(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types

type instr =
  | IBreak
  | IWeaken
  | IAssume of logic
  | IAssign of id * expr
  | IIf of logic * block * block
  | IIfNoElse of logic * block
  | IProbIf of prob_expr * block * block (* probability = (a,b), left commands, right commands *)
  | IWhile of logic * block
  | ILoop of block
  | ICall of id
  | ITick of int
  | ITickMemReads of id * int * bool * bool (* bits, host, read *)
  | ITickConflicts of id * int * bool (* bits, read *)
  | ICallArg of id list * id * id list (* function call with arguments and return variables *)
  | ICallUninterp of id * id * id list (* function call with arguments and return variables *)
  | IReturn of id list 

and block =
  { b_start_p: position
  ; b_end_p: position
  ; b_body: (instr * position) list
  }

type func = (Focus.focus, block) func_
