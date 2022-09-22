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

type 'a clval =
  | CVar of id
  | CArr of 'a clval * 'a cexpr list
  | CDeref of 'a clval
  | CRef of 'a clval
and 'a cexpr_ =
  | CL of 'a clval
  | CConst of const
  | CParam of cparam
  | CAdd of 'a cexpr * 'a cexpr
  | CSub of 'a cexpr * 'a cexpr
  | CMul of 'a cexpr * 'a cexpr
  | CDiv of 'a cexpr * 'a cexpr
  | CMod of 'a cexpr * 'a cexpr
  | CCall of string * 'a cexpr list
and 'a cexpr = 'a * 'a cexpr_

let edesc ((a, e) : 'a cexpr) : 'a cexpr_ = e
let eann ((a, e) : 'a cexpr) : 'a = a
let emk a e = (a, e)

let rec reannot_clval f (cl: 'a clval) =
    match cl with
    | CVar id -> CVar id
    | CArr (cl, es) -> CArr (reannot_clval f cl, List.map (reannot f) es)
    | CDeref cl -> CDeref (reannot_clval f cl)
    | CRef cl -> CRef (reannot_clval f cl)
            
and reannot (f: 'a -> 'b) (e: 'a cexpr) : 'b cexpr =
  let reannot_desc (d: 'a cexpr_) =
    match d with
    | CL cl -> CL (reannot_clval f cl)
    | CConst c -> CConst c
    | CParam p -> CParam p
    | CAdd (e1, e2) -> CAdd (reannot f e1, reannot f e2)
    | CSub (e1, e2) -> CSub (reannot f e1, reannot f e2)
    | CMul (e1, e2) -> CMul (reannot f e1, reannot f e2)
    | CDiv (e1, e2) -> CDiv (reannot f e1, reannot f e2)
    | CMod (e1, e2) -> CMod (reannot f e1, reannot f e2)
    | CCall (s, es) -> CCall (s, List.map (reannot f) es)
  in
  (f (eann e), reannot_desc (edesc e))

type 'a clogic =
  | CCmp of 'a cexpr * cmp * 'a  cexpr
  | CAnd of 'a clogic * 'a clogic
  | COr of 'a clogic * 'a clogic
  | CNot of 'a clogic

let rec reannot_clogic f l =
  match l with
  | CCmp (e1, c, e2) -> CCmp (reannot f e1, c, reannot f e2)
  | CAnd (l1, l2) -> CAnd (reannot_clogic f l1, reannot_clogic f l2)
  | COr (l1, l2) -> COr (reannot_clogic f l1, reannot_clogic f l2)
  | CNot l -> CNot (reannot_clogic f l)

type 'a cinstr =
  | CBreak
  | CDecl of id * mem * Cabs.base_type * int list
  (* dimensions of multi-dimensional arrays *)
  | CAssign of 'a clval * 'a cexpr * bool (* true = free *)
  | CIf of 'a clogic * 'a cblock * 'a cblock
  | CWhile of 'a clogic * 'a cblock
  | CFor of 'a cinstr list * 'a clogic * 'a cinstr list * 'a cblock
  | CReturn of 'a cexpr
  | CSync


        

(* Allows for annotations *)
and 'a cblock = 'a * 'a cinstr list

type 'a cfunc = Cabs.base_type * string * (id * Cabs.base_type) list *
                  'a cblock * bool
(* true = is a kernel *)

type 'a cprog = (id * Cabs.base_type * mem) list * 'a cfunc list

type annot = (unit cexpr * unit cexpr) option ref
              
let rec reannot_instr (f: annot -> unit) (i: annot cinstr) : unit cinstr =
  match i with
  | CBreak -> CBreak
  | CDecl (x, m, t, ds) -> CDecl (x, m, t, ds)
  | CAssign (x, e, free) -> CAssign (reannot_clval f x, reannot f e, free)
  | CIf (l, b1, b2) ->
     CIf (reannot_clogic f l, reannot_block f b1, reannot_block f b2)
  | CWhile (l, b) -> CWhile (reannot_clogic f l, reannot_block f b)
  | CFor (s1, l, s2, b) ->
     CFor (List.map (reannot_instr f) s1, reannot_clogic f l,
           List.map (reannot_instr f) s2,
           reannot_block f b)
  | CReturn e -> CReturn (reannot f e)
  | CSync -> CSync
         
and reannot_block f (a, b) =
  (f a, List.map (reannot_instr f) b)

let reannot_func f (ty, n, ps, b, k) =
  (ty, n, ps, reannot_block f b, k)


  
let reannot_prog (f: 'a -> unit) ((gs, fs) : annot cprog) : unit cprog =
  (gs, List.map (reannot_func f) fs)

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

let rec erase_ce e =
  emk ()
    (match edesc e with
     | CL cv -> CL (erase_cv cv)
     | CConst c -> CConst c
     | CParam p -> CParam p
     | CAdd (e1, e2) -> CAdd (erase_ce e1, erase_ce e2)
     | CSub (e1, e2) -> CSub (erase_ce e1, erase_ce e2)
     | CMul (e1, e2) -> CMul (erase_ce e1, erase_ce e2)
     | CDiv (e1, e2) -> CDiv (erase_ce e1, erase_ce e2)
     | CMod (e1, e2) -> CMod (erase_ce e1, erase_ce e2)
     | CCall (f, args) -> CCall (f, List.map erase_ce args)
    )
and erase_cv cv =
  match cv with
  | CVar x -> CVar x
  | CArr (cv, es) -> CArr (erase_cv cv, List.map erase_ce es)
  | CDeref cv -> CDeref (erase_cv cv)
  | CRef cv -> CRef (erase_cv cv)

let rec erase_cl l =
  match l with
  | CCmp (e1, c, e2) -> CCmp (erase_ce e1, c, erase_ce e2)
  | CAnd (l1, l2) -> CAnd (erase_cl l1, erase_cl l2)
  | COr (l1, l2) -> COr (erase_cl l1, erase_cl l2)
  | CNot l -> CNot (erase_cl l)

let rec erase_instr i = 
  (* emk ()  *)
  (match i with
   | CBreak -> CBreak
   | CSync -> CSync
   | CReturn e -> CReturn (erase_ce e)
   | CDecl (id, mem, bt, lst) -> CDecl (id, mem, bt, lst)
   | CAssign (cv, e, b) -> CAssign(erase_cv cv, erase_ce e, b)
   | CIf (cl, b1, b2) -> CIf(erase_cl cl, erase_block b1, erase_block b2)
   | CWhile (cl, b) -> CWhile(erase_cl cl, erase_block b)
   | CFor(b1, cl, b2, b3) -> CFor(List.map erase_instr b1, erase_cl cl, List.map erase_instr b2, erase_block b3))

and erase_block b = 
  let (_, b) = b in 
  emk () (List.map erase_instr b)
