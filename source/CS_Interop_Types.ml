(* Van Chan Ngo - 2017 *)

type csjump =
  | JJmp of int list                                            (* list of successor blocks *)
  | JRetVar of Types.id list                                    (* return variable for unvoid function *)
  | JRet of int                                                 (* for void function, e.g., return 0 *)

type csinstruction =
  | CSITick of int
  | CSIAssert of unit Types.logic                                    (* at the begining of a block, used for the block guards *)
  | CSIAssume of unit Types.logic                                    (* used for assumptions *)
  | CSIAssign of Types.id * unit Types.expr
  | CSICall of Types.id                                         (* no return and arguments, use global variables instead *)
  | CSICallArg of Types.id list * Types.id * Types.id list      (* function call with arguments and return variables *)
  | CSProbIf of Types.prob_expr                                 (* probability = (a,b), used for probabilistic branching *)

type csblock =
{
  bpreds : int list;
  binsts : csinstruction list;
  bjump : csjump
}

type 'a csfunc =
{
  fname : Types.id;
  flocs : Types.id list;                    (* local variables *)
  fargs  : Types.id list;                   (* arguments *)
  fbody : 'a
}

type cscallgraph = 
{
  cs_n : int;                               (* number of functions from 0 *)
  cs_edges : ((Types.id * int) list) array  (* list of node ids and their positions. *)
}

(* For example,
 * blkdesc = csblock
 * funcdesc = csblock array csfunc
 *)

module type CS_Querier = 
sig
  (* Module signature for code querier.
   * This is the module type that has to be
   * implemented by CodeSurfer.
   *)

  (* Conventions about block ids:
   *  - 0 is the id of the function start block
   *  - ids are a DFS numbering of the CFG
   *)

  type blkdesc
  type funcdesc

  val init:      bool -> string -> unit

  (* Program queries. *)
  val get_glos:  unit -> Types.id list            (* global variable list *)
  val get_main:  unit -> funcdesc                 (* return the definition of the main function, by convention its name is "start" *)
  val get_func:  Types.id -> funcdesc             (* return the definition of the function named id *)

  (* Function queries. *)
  val get_args:  funcdesc -> Types.id list        (* return the arguments *)
  val get_name:  funcdesc -> Types.id             (* return the name of function *)
  val get_locs:  funcdesc -> Types.id list        (* return the local variables *)
  val get_nblk:  funcdesc -> int                  (* return the # of blocks *)
  val get_blk:   funcdesc -> int -> blkdesc       (* return the definition of block with the provided id *)

  (* Block queries. *)
  val get_nins:  blkdesc -> int                   (* return # of instruction *)
  val get_ins:   blkdesc -> int -> csinstruction  (* return the instruction with the provided id, starting from 0 *) 
  val get_jmp:   blkdesc -> csjump                (* return the list of successor blocks or return *)
  val get_preds: blkdesc -> int list              (* return the list of predecessor blocks *)

end
