(* Stefan Muller - 2019 *)

open Types

module Int =
  struct
    type t = int
    let compare a b = a - b
  end
   
type dim = X | Y | Z
type mem = Local | Global | Shared | Host
type cparam =
  | GridDim of dim
  | BlockIdx of dim
  | BlockDim of dim
  | ThreadIdx of dim
  | WarpSize

type const =
  | CInt of int
  | CFloat of float
  | CString of string
  | CChar of char
  | CPtr of int * int (* address, offset *)

type clval =
  | CVar of id
  | CArr of clval * cexpr list
  | CDeref of clval
  | CRef of clval
and cexpr =
  | CL of clval
  | CConst of const
  | CParam of cparam
  | CAdd of cexpr * cexpr
  | CSub of cexpr * cexpr
  | CMul of cexpr * cexpr
  | CDiv of cexpr * cexpr
  | CMod of cexpr * cexpr
  | CCall of string * cexpr list

type clogic =
  | CCmp of cexpr * cmp * cexpr
  | CAnd of clogic * clogic
  | COr of clogic * clogic
  | CNot of clogic

type cinstr =
  | CBreak
  | CDecl of id * mem * Cabs.base_type * int list
  (* dimensions of multi-dimensional arrays *)
  | CAssign of clval * cexpr * bool (* true = free *)
  | CIf of clogic * cblock * cblock
  | CWhile of clogic * cblock
  | CFor of cinstr list * clogic * cinstr list * cblock
  | CReturn of cexpr
  | CSync



and cblock = cinstr list

type cfunc = string * (id * Cabs.base_type) list * cblock * bool
(* true = is a kernel *)

type cprog = (id * Cabs.base_type * mem) list * cfunc list

type arr_or_const = Arr of const array | Const of const
           
module Heap =
  struct
    module H = Map.Make(Int)

    type t = int * arr_or_const H.t

    let empty = (0, H.empty)

    let alloc (hs, h) a =
      (CPtr (hs, 0),
       (hs + 1, H.add hs (Arr a) h))

    let calloc (hs, h) c =
      (CPtr (hs, 0),
       (hs + 1, H.add hs (Const c) h))

    let lookup (addr, offset) (_, h) =
      let v =
        match H.find addr h with
        | Arr a -> a.(offset mod (Array.length a))
        | Const c -> c
      in
      ((* (match v with
        | CFloat f -> Printf.printf "%f\n" f
        | CInt n -> Printf.printf "%d\n" n
        | _ -> ()); *)
       v)

    let write (addr, offset) v (hs, h) =
      (match H.find_opt addr h with
       | Some (Arr a) ->
          (if offset >= Array.length a then
             let a' = Array.make (offset + 1) a.(0) in
             Array.blit a 0 a' 0 (Array.length a));
          (try a.(offset) <- v
           with Invalid_argument _ -> () )
       | _ -> ());
      (hs, h)
  end
