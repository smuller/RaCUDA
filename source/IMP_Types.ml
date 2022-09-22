(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types

type 'a instr =
  | IBreak
  | IWeaken
  | IAssume of 'a logic
  | IAssign of id * 'a expr
  | IIf of 'a logic * 'a block * 'a block
  | IIfNoElse of 'a logic * 'a block
  | IProbIf of prob_expr * 'a block * 'a block (* probability = (a,b), left commands, right commands *)
  | IWhile of 'a logic * 'a block
  | ILoop of 'a block
  | ICall of id
  | ITick of int
  | ITickMemReads of id * int * bool * bool (* bits, host, read *)
  | ITickConflicts of id * int * bool (* bits, read *)
  | ICallArg of id list * id * id list (* function call with arguments and return variables *)
  | ICallUninterp of id * id * id list (* function call with arguments and return variables *)
  | IReturn of id list 

and 'a block =
  { b_start_p: position
  ; b_end_p: position
  ; b_body: ('a instr * position) list
  ; annot: 'a
  }

type 'a func = (Focus.focus, 'a block) func_
