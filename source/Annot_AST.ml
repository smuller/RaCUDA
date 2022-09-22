(* Annotated CUDA ASTs *)
(* Stefan Muller - 2022 *)

open Graph
open CUDA_Types

module AnnotAST (A: AbsInt) =
  struct

    type acinstr =
      | ACBreak
      | ACDecl of id * mem * Cabs.base_type * int list
      (* dimensions of multi-dimensional arrays *)
      | ACAssign of clval * cexpr * bool (* true = free *)
      | ACIf of clogic * acblock * acblock
      | ACWhile of clogic * acblock
      | ACFor of cinstr list * aclogic * acinstr list * acblock
      | ACReturn of cexpr
      | ACSync

    and cblock = acinstr list * A.absval

    type acfunc =
      Cabs.base_type * string * (id * Cabs.base_type) list * acblock * bool
    (* true = is a kernel *)

    type acprog = (id * Cabs.base_type * mem) list * acfunc list
  end

module SimpleAnnotAST = AnnotAST(Graph_AI_Simple)
