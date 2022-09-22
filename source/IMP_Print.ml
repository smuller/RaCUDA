open Format
open Types
open IMP_Types

let ps = pp_print_string

let rec print_expr fmt e =
  match desc e with
  | EVar v -> ps fmt v
  | ENum n -> pp_print_int fmt n
  | EAdd (e1, e2) -> fprintf fmt "@[%a@ + %a@]" print_expr e1 print_expr e2
  | ESub (e1, e2) -> fprintf fmt "@[%a@ - %a@]" print_expr e1 print_expr e2
  | EMul (e1, e2) -> fprintf fmt "@[%a@ * %a@]" print_expr e1 print_expr e2
  | _ -> ()

let rec print_logic fmt l =
  let string_of_op = function Eq -> "==" | Le -> "<=" | Lt -> "<"
                              | Gt -> ">" | Ge -> ">=" | Ne -> "!="
  in
  match l with
  | LTrue -> ps fmt "true"
  | LFalse -> ps fmt "false"
  | LRandom -> ()
  | LCmp (e1, cmp, e2) ->
     fprintf fmt "@[%a@ %s %a@]" print_expr e1 (string_of_op cmp) print_expr e2
  | LAnd (e1, e2) -> fprintf fmt "@[%a@ && %a@]" print_logic e1 print_logic e2
  | LOr (e1, e2) -> fprintf fmt "@[%a@ || %a@]" print_logic e1 print_logic e2
  | LNot e -> fprintf fmt "!@[%a@]" print_logic e

let rec print_list fmt l =
  match l with
  | [] -> ()
  | [x] -> ps fmt x
  | x::t -> fprintf fmt "%s,@ %a" x print_list t

let rec print_instr fmt i =
  match i with
  | IBreak -> ps fmt "break;"
  | IWeaken -> ps fmt "weaken;"
  | IAssume l -> fprintf fmt "assume (@[%a@])" print_logic l
  | IAssign (v, e) ->
     fprintf fmt "@[<h>%s =@ %a;@]" v print_expr e
  | IIf (l, b1, b2) -> fprintf fmt "if (%a)@ %a@ else@ %a"
                         print_logic l
                         print_block b1
                         print_block b2
  | IIfNoElse (l, b) -> fprintf fmt "if (%a)@ %a@"
                         print_logic l
                         print_block b
  | IProbIf _ -> ()
  | IWhile (l, s) -> fprintf fmt "while (%a)@ %a"
                       print_logic l
                       print_block s
  | ILoop s -> fprintf fmt "loop@ %a" print_block s
  | ICall s -> fprintf fmt "%s();" s
  | ICallArg (rets, f, args) ->
     fprintf fmt "@[%a =@ %s(%a)@]" print_list rets f print_list args
  | ICallUninterp (ret, f, args) ->
     fprintf fmt "@[%s =@ %s(%a)@]" ret f print_list args
  | ITick n -> fprintf fmt "tick %d;" n
  | ITickMemReads (id, bits, host, read) ->
     fprintf fmt "%smem%ss(%s, %d)"
       (if host then "host" else "global")
       (if read then "read" else "write")
       id
       bits
  | ITickConflicts (id, bits, read) ->
     fprintf fmt "%sconflicts(%s, %d)"
       (if read then "read" else "write")
       id
       bits
  | IReturn ids -> fprintf fmt "return@ (%a)" print_list ids

and print_instr_list fmt l =
  List.iter (fun (i, _) -> fprintf fmt "%a@;" print_instr i) l
and print_block fmt b =
  fprintf fmt "@[<v 4>{@ %a@ }@]" print_instr_list b.b_body

let print_func fmt f =
  fprintf fmt "%s (%a)@ %a"
    f.fun_name
    print_list f.fun_vars
    print_block f.fun_body

let print_prog fmt (globals, funcs) =
  (print_list fmt globals;
   List.iter (fun f -> print_func fmt f; fprintf fmt "@;") funcs)
