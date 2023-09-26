(* Stefan Muller - 2019 *)

open Types
open CUDA_Types

module C = Cabs

exception InvalidCUDA of string * int * string

type intexp =
  | Expr of cexpr
  | Logic of clogic
  | Neither

let cmp_of_binop bo =
  match bo with
  | C.EQ -> Eq
  | C.NE -> Ne
  | C.LT -> Lt
  | C.LE -> Le
  | C.GT -> Gt
  | C.GE -> Ge
  | _ -> failwith "shouldn't happen"

let dims_of_type (f, l) t =
  let rec dtr t =
    match t with
    | C.ARRAY (t, e) -> e::(dtr t)
    | C.FLOAT _ -> []
    | C.CONST t -> dtr t
    | C.SHARED t -> dtr t
    | C.GLOBAL t -> dtr t
    | C.VOLATILE t -> dtr t
    | C.GNU_TYPE (_, t) -> dtr t
    | C.TYPE_LINE (_, _, t) -> dtr t
    | C.PTR t -> dtr t
    | C.RESTRICT_PTR t -> dtr t
    | _ -> []
  in
  match dtr t with
  | [] -> []
  | [x] -> []
  | x::t ->
     List.map (fun e ->
         match e with
         | C.CONSTANT (C.CONST_INT s) -> int_of_string s
         | _ -> raise (InvalidCUDA (f, l, "all but last dimension must be numbered")))
       t

let rec cuda_of_block (f, l) (d, b) =
  (List.concat (List.map (cuda_of_definition (f, l)) d) @
         (cuda_of_statement (f, l) b))
and cuda_of_statement (f, l) stmt =
  match stmt with
  | C.NOP -> []
  | C.DEFINITION d ->
     cuda_of_definition (f, l) d
  | C.COMPUTATION e ->
     let (ss, _) = cuda_of_expression (f, l) e in
     ss
  | C.BLOCK b -> cuda_of_block (f, l) b
  | C.SEQUENCE (s1, s2) ->
     (cuda_of_statement (f, l) s1) @ (cuda_of_statement (f, l) s2)
  | C.IF (e, s1, s2) ->
     let (s, lg) = logic_of_expr (f, l) e in
     s @ [CIf (lg, cuda_of_statement (f, l) s1, cuda_of_statement (f, l) s2)]
  | C.WHILE (e, s) ->
     let (ss, lg) = logic_of_expr (f, l) e in
     ss @ [CWhile (lg, cuda_of_statement (f, l) s)]
  | C.DOWHILE (e, s) ->
     (cuda_of_statement (f, l) s) @ (cuda_of_statement (f, l) (WHILE (e, s)))
  | C.FOR (e1, e2, e3, s) ->
     let (is1, e1) = cuda_of_expression (f, l) e1 in
     let (is2, lg) = logic_of_expr (f, l) e2 in
     let (is3, e3) = cuda_of_expression (f, l) e3 in
     is1 @ is2 @ [CFor ([], lg, is3, (cuda_of_statement (f, l) s) @ is2)]
  | C.BREAK -> [CBreak]
  | C.CONTINUE -> raise (InvalidCUDA (f, l, "continue not supported"))
  | C.RETURN e ->
     (match cuda_of_expression (f, l) e with
      | (ss, Expr e) -> ss @ [CReturn e]
      | _ -> raise (InvalidCUDA (f, l, "invalid return value")))
  | C.SWITCH _ -> raise (InvalidCUDA (f, l, "switch not supported yet"))
  | C.CASE _ -> raise (InvalidCUDA (f, l, "switch not supported yet"))
  | C.DEFAULT _ -> raise (InvalidCUDA (f, l, "switch not supported yet"))
  | C.LABEL (_, s) -> cuda_of_statement (f, l) s
  | C.GOTO _ -> raise (InvalidCUDA (f, l, "goto not supported, you monster"))
  | C.ASM _ -> raise (InvalidCUDA (f, l, "inline asm not supported"))
  | C.GNU_ASM _ -> raise (InvalidCUDA (f, l, "inline asm not supported"))
  | C.STAT_LINE (s, f, l) -> (Printf.printf "line\n";
                              cuda_of_statement (f, l) s)

and cuda_of_expression (f, l) expr =
  let (ss, e) = rev_cuda_of_expression (f, l) expr in
  (List.rev ss, e)
and logic_of_expr (f, l) expr =
  match cuda_of_expression (f, l) expr with
  | (ss, Logic l) -> (ss, l)
  | _ -> raise (InvalidCUDA (f, l, "boolean expression expected"))
and assign_of_expr (f, l) expr =
  match rev_cuda_of_expression (f, l) expr with
  | (ss, _) -> List.rev ss
  | _ -> raise (InvalidCUDA (f, l, "assignment expected"))
and rev_cuda_of_expression (f, l) expr =
  match expr with
  | C.NOTHING -> ([], Neither)
  | C.UNARY (uo, e) ->
     (match (uo, rev_cuda_of_expression (f, l) e) with
      | (C.MINUS, (ss, Expr ce)) -> (ss, Expr (CSub (CConst (CInt 0), ce)))
      | (C.PLUS, (ss, Expr ce)) -> (ss, Expr ce)
      | (C.NOT, (ss, Logic l)) -> (ss, Logic (CNot l))
      | (C.BNOT, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.MEMOF, (ss, Expr (CL lv))) -> (ss, Expr (CL (CDeref lv)))
      | (C.ADDROF, (ss, Expr (CL lv))) -> (ss, Expr (CL (CDeref lv)))
      | (C.PREINCR, (ss, Expr (CL lv))) ->
         ((CAssign (lv, CAdd (CL lv, (CConst (CInt 1))), false))::ss,
          Expr (CL lv))
      | (C.PREDECR, (ss, Expr (CL lv))) ->
         ((CAssign (lv, CSub (CL lv, (CConst (CInt 1))), false))::ss,
          Expr (CL lv))
      | (C.POSINCR, (ss, Expr (CL lv))) ->
         ((CAssign (lv, CAdd (CL lv, (CConst (CInt 1))), false))::ss,
          Expr (CSub (CL lv, CConst (CInt 1))))
      | (C.POSDECR, (ss, Expr (CL lv))) ->
         ((CAssign (lv, CSub (CL lv, (CConst (CInt 1))), false))::ss,
          Expr (CAdd (CL lv, CConst (CInt 1))))
      | _ -> raise (InvalidCUDA (f, l, "invalid unary operation")))
  | BINARY (bo, e1, e2) ->
     (match (bo, rev_cuda_of_expression (f, l) e1, rev_cuda_of_expression (f, l) e2)
      with
      | (C.ADD, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Expr (CAdd (ce1, ce2)))
      | (C.SUB, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Expr (CSub (ce1, ce2)))
      | (C.MUL, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Expr (CMul (ce1, ce2)))
      | (C.DIV, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Expr (CDiv (ce1, ce2)))
      | (C.MOD, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Expr (CMod (ce1, ce2)))

      | (C.AND, (s1, Logic l1), (s2, Logic l2)) ->
         (s2 @ s1, Logic (CAnd (l1, l2)))
      | (C.OR, (s1, Logic l1), (s2, Logic l2)) ->
         (s2 @ s1, Logic (COr (l1, l2)))

      | (C.BAND, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.BOR, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.XOR, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))

      | (C.SHL, (s1, Expr ce1), (s2, Expr (CConst (CInt n)))) ->
         (s2 @ s1, Expr (CMul (ce1, CConst (CInt (2 lsl (n - 1))))))
      | (C.SHL, _, _) ->
         raise (InvalidCUDA (f, l, "only shifts by constant allowed"))
      | (C.SHR, (s1, Expr ce1), (s2, Expr (CConst (CInt n)))) ->
         (s2 @ s1, Expr (CDiv (ce1, CConst (CInt (2 lsl (n - 1))))))
      | (C.SHR, _, _) ->
         raise (InvalidCUDA (f, l, "only shifts by constant allowed"))

      | (C.EQ, (s1, Expr ce1), (s2, Expr ce2))
        | (C.NE, (s1, Expr ce1), (s2, Expr ce2))
        | (C.LT, (s1, Expr ce1), (s2, Expr ce2))
        | (C.GT, (s1, Expr ce1), (s2, Expr ce2))
        | (C.LE, (s1, Expr ce1), (s2, Expr ce2))
        | (C.GE, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, Logic (CCmp (ce1, cmp_of_binop bo, ce2)))

      | (C.ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, ce2, false))::(s2 @ s1), Expr ce2)
      | (C.ADD_ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, CAdd (CL lv, ce2), false))::(s2 @ s1),
          Expr (CAdd (CL lv, ce2)))
      | (C.SUB_ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, CSub (CL lv, ce2), false))::(s2 @ s1),
          Expr (CSub (CL lv, ce2)))
      | (C.MUL_ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, CMul (CL lv, ce2), false))::(s2 @ s1),
          Expr (CMul (CL lv, ce2)))
      | (C.DIV_ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, CDiv (CL lv, ce2), false))::(s2 @ s1),
          Expr (CDiv (CL lv, ce2)))
      | (C.MOD_ASSIGN, (s1, Expr (CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, CMod (CL lv, ce2), false))::(s2 @ s1),
          Expr (CMod (CL lv, ce2)))
      | (C.BAND_ASSIGN, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.BOR_ASSIGN, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.XOR_ASSIGN, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.SHL_ASSIGN, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.SHR_ASSIGN, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | _ -> raise (InvalidCUDA (f, l, "unsupported binary operation")))

  | C.QUESTION (e1, e2, e3) ->
     raise (InvalidCUDA (f, l, "ternary operator not yet supported"))
  | C.CAST (_, e) -> rev_cuda_of_expression (f, l) e
  | C.CALL (VARIABLE "__syncthreads", _) -> ([CSync], Neither)
  | C.CALL (VARIABLE func, args) ->
     let (ss, args) = List.fold_left
                  (fun (ss, es) e ->
                    match (rev_cuda_of_expression (f, l) e) with
                    | (ss', Expr ce) -> (ss' @ ss, ce::es)
                    | _ -> raise
                             (InvalidCUDA (f, l,
                                           "unsupported function arguments")))
                  ([], [])
                  args
     in
     (ss, Expr (CCall (func, List.rev args)))
  | C.CALL _ -> raise (InvalidCUDA (f, l, "unsupported function call"))
  | C.COMMA es ->
     List.fold_left
       (fun (ss, es) e -> let (ss', e') = rev_cuda_of_expression (f, l) e in
                          (ss' @ ss, e'))
       ([], Neither)
       es
  | C.CONSTANT c -> ([], Expr (CConst (cuda_of_constant (f, l) c)))
  | C.VARIABLE x ->
     ([], Expr
            (if String.compare x "warpSize" = 0 then CParam (WarpSize)
             else CL (CVar x)))
  | C.EXPR_SIZEOF _ -> raise (InvalidCUDA (f, l, "sizeof not supported"))
  | C.TYPE_SIZEOF _ -> raise (InvalidCUDA (f, l, "sizeof not supported"))
  | C.INDEX (e1, e2) ->
     (match (rev_cuda_of_expression (f, l) e1, rev_cuda_of_expression (f, l) e2) with
      | ((ss1, Expr (CL a)), (ss2, Expr ce2)) ->
         (ss2 @ ss1, Expr (CL (CArr (a, [ce2]))))
      | _ -> raise (InvalidCUDA (f, l, "unsupported array access")))
  | C.MEMBEROF (e, s) | C.MEMBEROFPTR (e, s) ->
     (match (rev_cuda_of_expression (f, l) e) with
     | (ss, Expr (CL (CVar a))) ->
        (match cuda_of_param (f, l) (a, s) with
         | Some p -> (ss, Expr (CParam p))
         | None -> (ss, Expr (CL (CVar (a ^ "." ^ s)))))
     | _ -> raise (InvalidCUDA (f, l, "unsupported member access")))
  | C.GNU_BODY _ ->
        raise (InvalidCUDA (f, l, "gnubody not supported... whatever that is"))
  | C.EXPR_LINE (e, f, l) -> rev_cuda_of_expression (f, l) e

and cuda_of_constant (f, l) c =
  try
    match c with
    | C.CONST_INT s -> CInt (int_of_string s)
    | C.CONST_FLOAT s -> CFloat (float_of_string s)
    | C.CONST_STRING s -> CString s
    | C.CONST_CHAR s -> CChar (String.get s 0)
    | C.CONST_COMPOUND _ ->
       raise (InvalidCUDA (f, l, "array literal initialization not supported"))
  with
    _ -> raise (InvalidCUDA (f, l, "unsupported constant"))

and cuda_of_param (f, l) (s1, s2) =
  let dim s =
    match s with
    | "x" | "X" -> Some X
    | "y" | "Y" -> Some Y
    | "z" | "Z" -> Some Z
    | _ -> None
  in
  match (s1, dim s2) with
  | (_, None) -> None
  | ("gridDim", Some d) -> Some (GridDim d)
  | ("blockIdx", Some d) -> Some (BlockIdx d)
  | ("blockDim", Some d) -> Some (BlockDim d)
  | ("threadIdx", Some d) -> Some (ThreadIdx d)
  | _ -> None

and cuda_of_definition (f, l) d =
  match d with
  | C.DECDEF (t, _, ids) ->
     let mem = mod_of_type false t in
     List.concat (
         List.map (fun (id, t, _, e) ->
             let dims = dims_of_type (f, l) t in
             match e with
             | C.NOTHING -> [CDecl (id, mem, t, dims)]
             | _ -> (match cuda_of_expression (f, l) e with
                     | (ss, Expr e) ->
                        ss @ [CDecl (id, mem, t, dims); CAssign (CVar id, e, false)]
                     | _ ->
                        raise (InvalidCUDA (f, l, "invalid initialization"))))
           ids)
  | _ -> raise (InvalidCUDA (f, l, "invalid definition inside function"))

and mod_of_type host t =
  match t with
  | C.CONST t -> mod_of_type host t
  | C.SHARED _ -> Shared
  | C.GLOBAL _ -> Global
  | C.VOLATILE t -> mod_of_type host t
  | C.TYPE_LINE (_, _, t) -> mod_of_type host t
  | _ -> if host then Host else Local

and handle_proto (f, l) t =
  match t with
  | C.CONST t
    | C.VOLATILE t
    | C.SHARED t
    | C.PTR t
    | C.RESTRICT_PTR t
    | C.GNU_TYPE (_, t)  -> handle_proto (f, l) t
  | C.TYPE_LINE (f, l, t) -> handle_proto (f, l) t
  | C.GLOBAL t ->
     let (names, _) = handle_proto (f, l) t in
     (names, true)
  | C.PROTO (_, names, _) ->
     (List.map (fun (_, _, (s, t, _, _)) -> (s, t)) names, false)
(*  | C.OLD_PROTO (_, names, _) ->
     (names, false) *)
  | VOID -> Printf.printf "void?\n";
            raise (InvalidCUDA (f, l, "invalid definition"))
  | _ -> raise (InvalidCUDA (f, l, "invalid definition"))

and cuda_of_topdef (f, l) d =
  match d with
  | C.FUNDEF ((_, _, (name, t, _, _)), s) ->
     let (params, is_kernel) = handle_proto (f, l) t in
     ([], [name, params, cuda_of_block (f, l) s, is_kernel])
  | C.OLDFUNDEF ((_, _, (name, t, _, _)), _, s) ->
     let (params, is_kernel) = handle_proto (f, l) t in
     ([], [name, params, cuda_of_block (f, l) s, is_kernel])
  | C.DECDEF (t, _, ids) ->
     let mem = mod_of_type true t in
     (List.map (fun (id, t, _, _) -> (id, t, mem)) ids, [])
  | C.TYPEDEF _ -> ([], [])
  | C.ONLYTYPEDEF _ -> ([], [])

and cuda_of_file name ds =
  List.fold_left
    (fun (g, fn) d ->
      let (g', fn') = cuda_of_topdef (name, 0) d in
      (g @ g', fn @ fn'))
    ([], [])
    ds

open Format
let ps = pp_print_string
let pi = pp_print_int
let pf = pp_print_float
let pc = pp_print_char

let print_param fmt p =
  let print_dim fmt d =
    match d with
    | X -> ps fmt "x"
    | Y -> ps fmt "y"
    | Z -> ps fmt "z"
  in
  match p with
  | GridDim d -> (ps fmt "gridDim."; print_dim fmt d)
  | BlockIdx d -> (ps fmt "blockIdx."; print_dim fmt d)
  | BlockDim d -> (ps fmt "blockDim."; print_dim fmt d)
  | ThreadIdx d -> (ps fmt "threadIdx."; print_dim fmt d)
  (*  | ThreadId -> ps fmt "threadId"*)
  | WarpSize -> ps fmt "warpSize"

let print_const fmt c =
  match c with
  | CInt i -> pi fmt i
  | CFloat f -> pf fmt f
  | CString s -> ps fmt s
  | CChar c -> pc fmt c
  | CPtr _ -> ps fmt "<addr>"

let rec print_clval fmt e =
  match e with
  | CVar id -> ps fmt id
  | CArr (a, es) ->
     (fprintf fmt "%a@," print_clval a;
      List.iter (fun e -> fprintf fmt "[%a]" print_cexpr e) es)
  | CDeref v -> fprintf fmt "*(%a)" print_clval v
  | CRef v -> fprintf fmt "&(%a)" print_clval v
and print_args fmt l =
  match l with
  | [] -> ()
  | [x] -> print_cexpr fmt x
  | a::t -> fprintf fmt "%a,@ %a" print_cexpr a print_args t
and print_cexpr fmt e =
  match e with
  | CL v -> print_clval fmt v
  | CConst c -> print_const fmt c
  | CParam p -> print_param fmt p
  | CAdd (e1, e2) -> fprintf fmt "@[%a@ + %a@]" print_cexpr e1 print_cexpr e2
  | CSub (e1, e2) -> fprintf fmt "@[%a@ - %a@]" print_cexpr e1 print_cexpr e2
  | CMul (e1, e2) -> fprintf fmt "@[%a@ * %a@]" print_cexpr e1 print_cexpr e2
  | CDiv (e1, e2) -> fprintf fmt "@[%a@ / %a@]" print_cexpr e1 print_cexpr e2
  | CMod (e1, e2) -> fprintf fmt "@[%a@ %% %a@]" print_cexpr e1 print_cexpr e2
  | CCall (s, args) ->
     fprintf fmt "@[%s(%a)@]" s print_args args

let rec print_clogic fmt l =
  let string_of_op = function Eq -> "==" | Le -> "<=" | Lt -> "<"
                              | Gt -> ">" | Ge -> ">=" | Ne -> "!="
  in
  match l with
  | CCmp (e1, cmp, e2) ->
     fprintf fmt "@[%a@ %s %a@]" print_cexpr e1 (string_of_op cmp) print_cexpr e2
  | CAnd (e1, e2) -> fprintf fmt "@[%a@ && %a@]" print_clogic e1 print_clogic e2
  | COr (e1, e2) -> fprintf fmt "@[%a@ || %a@]" print_clogic e1 print_clogic e2
  | CNot e -> fprintf fmt "!@[%a@]" print_clogic e

let string_of_mem = function Local -> "__local__"
                           | Global -> "__global__"
                           | Shared -> "__shared__"
                           | Host -> ""

let rec print_cinstr fmt i =
  match i with
  | CBreak -> ps fmt "break;"
  | CDecl (id, mem, _, _) -> fprintf fmt "%s %s;" (string_of_mem mem) id
  | CAssign (lv, e, free) ->
     fprintf fmt "@[<h>%a %s=@ %a;@]" print_clval lv
       (if free then "0" else "")
       print_cexpr e
  | CIf (l, s1, s2) -> fprintf fmt "if (%a)@ %a@ else@ %a"
                         print_clogic l
                         print_cblock s1
                         print_cblock s2
  | CWhile (l, s) -> fprintf fmt "while (%a)@ %a"
                       print_clogic l
                       print_cblock s
  | CFor (s1, l, s2, s) -> fprintf fmt
                           "@[<h>@]for (@[<h>%a@];@ %a;@ @[<h>%a@])@]@; %a"
                             print_instr_list s1
                             print_clogic l
                             print_instr_list s2
                             print_cblock s
  | CReturn e -> fprintf fmt "return %a;" print_cexpr e

and print_instr_list fmt l =
  List.iter (fun i -> printf "%a@;" print_cinstr i) l
and print_cblock fmt b =
  fprintf fmt "@[<v 4>{@ %a@ }@]" print_instr_list b

and print_ids fmt l =
  match l with
  | [] -> ()
  | [x] -> ps fmt x
  | a::t -> fprintf fmt "%a,@ %a" ps a print_ids t

let print_cfunc fmt (name, args, b, kernel) =
  fprintf fmt "%s%s (%a)@ %a"
    (if kernel then "__global__ " else "")
    name
    print_ids (List.map fst args)
    print_cblock b

let print_cprog fmt (globals, funcs) =
  (List.iter (fun (id, _, mem) -> fprintf fmt "%s %s;@;" (string_of_mem mem) id)
     globals;
   List.iter (fun f -> print_cfunc fmt f; fprintf fmt "@;") funcs)

let next_id = ref 0
let new_var () =
  let id = !next_id in
  let _ = next_id := id + 1 in
  "__temp" ^ (string_of_int id)

let rec collapse_arrays lv =
  match lv with
  | CVar _ -> lv
  | CArr (lv, es) ->
     (match collapse_arrays lv with
      | CArr (lv', es') -> CArr (lv', es' @ es)
      | lv' -> CArr (lv', es))
  | CDeref lv -> CDeref (collapse_arrays lv)
  | CRef lv -> CRef (collapse_arrays lv)

module M = Map.Make(Id)
let submap : expr M.t ref = ref M.empty
let add_var_exp x e =
  if Str.string_match (Str.regexp_string "__temp") x 0 ||
       Str.string_match (Str.regexp_string "_reg_") x 0 then
    ((* Printf.printf "Adding %s\n" x; *)
      submap := M.add x e (!submap))
let rec lookup_var x =
  match M.find_opt x (!submap) with
  | None -> EVar x
  | Some e -> lookup_exp e
and lookup_exp e =
  match e with
  | EVar x -> lookup_var x
  | EAdd (e1, e2) -> EAdd (lookup_exp e1, lookup_exp e2)
  | ESub (e1, e2) -> ESub (lookup_exp e1, lookup_exp e2)
  | EMul (e1, e2) -> EMul (lookup_exp e1, lookup_exp e2)
  | _ -> e

let rec lookup_poly p =
  let open Polynom in
  Poly.fold (fun m c p ->
      Poly.add p (Poly.mul
                    (Monom.fold
                       (fun f d p ->
                         Poly.mul p
                           (match f with
                            | Var x ->
                               Poly.pow d (Poly.of_expr (lookup_var x))
                            | Max p -> Poly.of_monom
                                         (Monom.of_factor
                                            (Factor.Max (lookup_poly p))
                                            d)
                                         1.0))
                       m
                       (Poly.const 1.0))
                    (Poly.const c)))
    p
    (Poly.zero ())

let rec normalize_clval lv =
  match lv with
  | CVar id -> ([], id)
  | CArr (lv, es) ->
     let (is1, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', x) = normalize_cexpr e in
           (is' @ is, (CL (CVar x))::xs))
         ([], [])
         es
     in
     let (is2, x2) = normalize_clval lv in
     let x = new_var () in
     ((CAssign (CVar x, (CL (CArr (CVar x2, xs))), true))::
        (is1 @ is2),
      x)
  | CDeref lv ->
     let (is, x1) = normalize_clval lv in
     let x = new_var () in
     ((CAssign (CVar x, (CL (CDeref (CVar x1))), true))::is, x)
  | CRef lv ->
     let (is, x1) = normalize_clval lv in
     let x = new_var () in
     ((CAssign (CVar x, (CL (CRef (CVar x1))), true))::is, x)
and normalize_clval_write lv =
  match lv with
  | CVar id -> ([], CVar id)
  | CArr (lv, es) ->
     let (is1, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', x) = normalize_cexpr e in
           (is' @ is, (CL (CVar x))::xs))
         ([], [])
         es
     in
     let (is2, x2) = normalize_clval lv in
     let x = new_var () in
     (is1 @ is2, CArr (CVar x2, xs))
  | CDeref lv ->
     let (is, x1) = normalize_clval lv in
     (is, CDeref (CVar x1))
  | CRef lv ->
     let (is, x1) = normalize_clval lv in
     (is, CRef (CVar x1))
and normalize_cexpr e =
  match e with
  | CL lv -> normalize_clval (collapse_arrays lv)
  | CConst _ | CParam _ ->
     let x = new_var () in
     ([CAssign (CVar x, e, true)], x)
  | CAdd (e1, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (EAdd (EVar x1, EVar x2)) in
     ((CAssign (CVar x, CAdd (CL (CVar x1), CL (CVar x2)), true))::(is2 @ is1),
      x)
  | CSub (e1, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (ESub (EVar x1, EVar x2)) in
     ((CAssign (CVar x, CSub (CL (CVar x1), CL (CVar x2)), true))::(is2 @ is1),
      x)
  | CMul (e1, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (EMul (EVar x1, EVar x2)) in
     ((CAssign (CVar x, CMul (CL (CVar x1), CL (CVar x2)), true))::(is2 @ is1),
      x)
  | CDiv (e1, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     ((CAssign (CVar x, CDiv (CL (CVar x1), CL (CVar x2)), true))::(is2 @ is1),
      x)
  | CMod (e1, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     ((CAssign (CVar x, CMod (CL (CVar x1), CL (CVar x2)), true))::(is2 @ is1),
      x)
  | CCall (s, es) ->
     let (is, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', x) = normalize_cexpr e in
           (is' @ is, (CL (CVar x))::xs))
         ([], [])
         es
     in
     let x = new_var () in
     ((CAssign (CVar x, CCall (s, xs), true))::is, x)

let rec normalize_clogic l =
  match l with
  | CCmp (e1, c, e2) ->
     let (is1, x1) = normalize_cexpr e1 in
     let (is2, x2) = normalize_cexpr e2 in
     (is2 @ is1, CCmp (CL (CVar x1), c, CL (CVar x2)))
  | CAnd (l1, l2) ->
     let (is1, l1) = normalize_clogic l1 in
     let (is2, l2) = normalize_clogic l2 in
     (is2 @ is1, CAnd (l1, l2))
  | COr (l1, l2) ->
     let (is1, l1) = normalize_clogic l1 in
     let (is2, l2) = normalize_clogic l2 in
     (is2 @ is1, COr (l1, l2))
  | CNot l ->
     let (is, l) = normalize_clogic l in
     (is, CNot l)

let rec normalize_cinstr i =
  match i with
  | CBreak | CDecl _ -> [i]
  | CAssign (lv, e, b) ->
     let (is1, lv') = normalize_clval_write (collapse_arrays lv) in
     let (is2, y) = normalize_cexpr e in
     (List.rev is1) @ (List.rev is2) @
       [CAssign (lv', CL (CVar y), false)]
  | CIf (l, s1, s2) ->
     let (is1, l) = normalize_clogic l in
     let s1 = normalize_block s1 in
     let s2 = normalize_block s2 in
     (List.rev is1) @ [CIf (l, s1, s2)]
  | CWhile (l, s) ->
     let (is, l) = normalize_clogic l in
     let s = normalize_block s in
     (List.rev is) @ [CWhile (l, s @ is)]
  | CFor (s1, l, s2, s3) ->
     let s1 = normalize_block s1 in
     let (is, l) = normalize_clogic l in
     let s2 = normalize_block s2 in
     let s3 = normalize_block s3 in
     s1 @ (List.rev is) @ [CFor ([], l, s2, s3 @ is)]
  | CReturn e ->
     let (is, x) = normalize_cexpr e in
     [CReturn (CL (CVar x))]
  | CSync -> [CSync]
and normalize_block b =
  List.concat (List.map normalize_cinstr b)

let normalize_func (s, ids, b, is_kernel) =
  (s, ids, normalize_block b, is_kernel)

let normalize_prog (l, funcs) = (l, List.map normalize_func funcs)
