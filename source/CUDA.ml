(* Stefan Muller - 2019 *)

open Types
open CUDA_Types

module C = Cabs

exception InvalidCUDA of string * int * string

type 'a intexp =
  | Expr of 'a cexpr
  | Logic of 'a clogic
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

let add_to_block (a, b) s =
  (a, b @ s)
    
let rec cuda_of_block da (f, l) (d, b) =
  (da (), (List.concat (List.map (cuda_of_definition da (f, l)) d)) @
         (cuda_of_statement da (f, l) b))
and cuda_of_stmt_or_block da (f, l) stmt =
  match stmt with
  | C.BLOCK b -> cuda_of_block da (f, l) b
  | _ -> (da (), cuda_of_statement da (f, l) stmt)
and cuda_of_statement da (f, l) stmt =
  let cuda_of_statement = cuda_of_statement da in
  let cuda_of_expression = cuda_of_expression da in
  match stmt with
  | C.NOP -> []
  | C.DEFINITION d ->
     cuda_of_definition da (f, l) d
  | C.COMPUTATION e ->
     let (ss, _) = cuda_of_expression (f, l) e in
     ss
  | C.BLOCK b -> snd (cuda_of_block da (f, l) b)
  | C.SEQUENCE (s1, s2) ->
     (cuda_of_statement (f, l) s1) @ (cuda_of_statement (f, l) s2)
  | C.IF (e, s1, s2) ->
     let (s, lg) = logic_of_expr da (f, l) e in
     s @ [CIf (lg, cuda_of_stmt_or_block da (f, l) s1,
               cuda_of_stmt_or_block da (f, l) s2)]
  | C.WHILE (e, s) ->
     let (ss, lg) = logic_of_expr da (f, l) e in
     ss @ [CWhile (lg, cuda_of_stmt_or_block da (f, l) s)]
  | C.DOWHILE (e, s) ->
     (cuda_of_statement (f, l) s) @ (cuda_of_statement (f, l) (WHILE (e, s)))
  | C.FOR (e1, e2, e3, s) ->
     let (is1, e1) = cuda_of_expression (f, l) e1 in
     let (is2, lg) = logic_of_expr da (f, l) e2 in
     let (is3, e3) = cuda_of_expression (f, l) e3 in
  (* is1 @ is2 @ [CFor ([], lg, is3, (cuda_of_statement (f, l) s) @ is2)] *)
     [CFor (is1 @ is2, lg, is3,
            add_to_block (cuda_of_stmt_or_block da (f, l) s) is2)]
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

and cuda_of_expression da (f, l) expr =
  let (ss, e) = rev_cuda_of_expression da (f, l) expr in
  (List.rev ss, e)
and logic_of_expr da (f, l) expr =
  match cuda_of_expression da (f, l) expr with
  | (ss, Logic l) -> (ss, l)
  | _ -> raise (InvalidCUDA (f, l, "boolean expression expected"))
and assign_of_expr da (f, l) expr =
  match rev_cuda_of_expression da (f, l) expr with
  | (ss, _) -> List.rev ss
  | _ -> raise (InvalidCUDA (f, l, "assignment expected"))
and rev_cuda_of_expression da (f, l) exp =
  let rev_cuda_of_expression = rev_cuda_of_expression da in
  let expr e = Expr (da (), e) in
  let logic e = Logic e in
  let mk ed = (da (), ed) in
  let cadd (e1, e2) = mk (CAdd (e1, e2)) in
  let csub (e1, e2) = mk (CSub (e1, e2)) in
  let cl lv = mk (CL lv) in
  let cconst c = mk (CConst c) in
  match exp with
  | C.NOTHING -> ([], Neither)
  | C.UNARY (uo, e) ->
     (match (uo, rev_cuda_of_expression (f, l) e) with
      | (C.MINUS, (ss, Expr ce)) -> (ss, expr (CSub (cconst (CInt 0), ce)))
      | (C.PLUS, (ss, Expr ce)) -> (ss, Expr ce)
      | (C.NOT, (ss, Logic l)) -> (ss, logic (CNot l))
      | (C.BNOT, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.MEMOF, (ss, Expr (a, CL lv))) -> (ss, Expr (a, CL (CDeref lv)))
      | (C.ADDROF, (ss, Expr (a, CL lv))) -> (ss, Expr (a, CL (CDeref lv)))
      | (C.PREINCR, (ss, Expr (a, CL lv))) ->
         ((CAssign (lv, cadd (cl lv, (cconst (CInt 1))), false))::ss,
          Expr (a, CL lv))
      | (C.PREDECR, (ss, Expr (a, CL lv))) ->
         ((CAssign (lv, csub (cl lv, (cconst (CInt 1))), false))::ss,
          Expr (a, CL lv))
      | (C.POSINCR, (ss, Expr (a, CL lv))) ->
         ((CAssign (lv, cadd (cl lv, (cconst (CInt 1))), false))::ss,
          Expr (csub (cl lv, cconst (CInt 1))))
      | (C.POSDECR, (ss, Expr (_, CL lv))) ->
         ((CAssign (lv, csub (cl lv, (cconst (CInt 1))), false))::ss,
          Expr (cadd (cl lv, cconst (CInt 1))))
      | _ -> raise (InvalidCUDA (f, l, "invalid unary operation")))
  | BINARY (bo, e1, e2) ->
     (match (bo, rev_cuda_of_expression (f, l) e1, rev_cuda_of_expression (f, l) e2)
      with
      | (C.ADD, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, expr (CAdd (ce1, ce2)))
      | (C.SUB, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, expr (CSub (ce1, ce2)))
      | (C.MUL, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, expr (CMul (ce1, ce2)))
      | (C.DIV, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, expr (CDiv (ce1, ce2)))
      | (C.MOD, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, expr (CMod (ce1, ce2)))

      | (C.AND, (s1, Logic l1), (s2, Logic l2)) ->
         (s2 @ s1, logic (CAnd (l1, l2)))
      | (C.OR, (s1, Logic l1), (s2, Logic l2)) ->
         (s2 @ s1, logic (COr (l1, l2)))

      | (C.BAND, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.BOR, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))
      | (C.XOR, _, _) ->
         raise (InvalidCUDA (f, l, "bitwise operations not supported"))

      | (C.SHL, (s1, Expr ce1), (s2, Expr (_, CConst (CInt n)))) ->
         (s2 @ s1, expr (CMul (ce1, cconst (CInt (2 lsl (n - 1))))))
      | (C.SHL, _, _) ->
         raise (InvalidCUDA (f, l, "only shifts by constant allowed"))
      | (C.SHR, (s1, Expr ce1), (s2, Expr (_, CConst (CInt n)))) ->
         (s2 @ s1, expr (CDiv (ce1, cconst (CInt (2 lsl (n - 1))))))
      | (C.SHR, _, _) ->
         raise (InvalidCUDA (f, l, "only shifts by constant allowed"))

      | (C.EQ, (s1, Expr ce1), (s2, Expr ce2))
        | (C.NE, (s1, Expr ce1), (s2, Expr ce2))
        | (C.LT, (s1, Expr ce1), (s2, Expr ce2))
        | (C.GT, (s1, Expr ce1), (s2, Expr ce2))
        | (C.LE, (s1, Expr ce1), (s2, Expr ce2))
        | (C.GE, (s1, Expr ce1), (s2, Expr ce2)) ->
         (s2 @ s1, logic (CCmp (ce1, cmp_of_binop bo, ce2)))

      | (C.ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, ce2, false))::(s2 @ s1), Expr ce2)
      | (C.ADD_ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, cadd (cl lv, ce2), false))::(s2 @ s1),
          expr (CAdd (cl lv, ce2)))
      | (C.SUB_ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, csub (cl lv, ce2), false))::(s2 @ s1),
          expr (CSub (cl lv, ce2)))
      | (C.MUL_ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, mk (CMul (cl lv, ce2)), false))::(s2 @ s1),
          expr (CMul (cl lv, ce2)))
      | (C.DIV_ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, mk (CDiv (cl lv, ce2)), false))::(s2 @ s1),
          expr (CDiv (cl lv, ce2)))
      | (C.MOD_ASSIGN, (s1, Expr (_, CL lv)), (s2, Expr ce2)) ->
         ((CAssign (lv, mk (CMod (cl lv, ce2)), false))::(s2 @ s1),
          expr (CMod (cl lv, ce2)))
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
     (match func with
      | "min" -> (ss, expr (CMin (List.rev args)))
      | "max" -> (ss, expr (CMax (List.rev args)))
      | _ -> (ss, expr (CCall (func, List.rev args)))
     )
  | C.CALL _ -> raise (InvalidCUDA (f, l, "unsupported function call"))
  | C.COMMA es ->
     List.fold_left
       (fun (ss, es) e -> let (ss', e') = rev_cuda_of_expression (f, l) e in
                          (ss' @ ss, e'))
       ([], Neither)
       es
  | C.CONSTANT c -> ([], expr (CConst (cuda_of_constant (f, l) c)))
  | C.VARIABLE x ->
     ([], expr
            (if String.compare x "warpSize" = 0 then CParam (WarpSize)
             else CL (CVar x)))
  | C.EXPR_SIZEOF _ -> raise (InvalidCUDA (f, l, "sizeof not supported"))
  | C.TYPE_SIZEOF _ -> raise (InvalidCUDA (f, l, "sizeof not supported"))
  | C.INDEX (e1, e2) ->
     (match (rev_cuda_of_expression (f, l) e1, rev_cuda_of_expression (f, l) e2) with
      | ((ss1, Expr (an, CL a)), (ss2, Expr ce2)) ->
         (ss2 @ ss1, Expr (an, CL (CArr (a, [ce2]))))
      | _ -> raise (InvalidCUDA (f, l, "unsupported array access")))
  | C.MEMBEROF (e, s) | C.MEMBEROFPTR (e, s) ->
     (match (rev_cuda_of_expression (f, l) e) with
     | (ss, Expr (_, CL (CVar a))) ->
        (match cuda_of_param (f, l) (a, s) with
         | Some p -> (ss, expr (CParam p))
         | None -> (ss, expr (CL (CVar (a ^ "." ^ s)))))
     | _ -> raise (InvalidCUDA (f, l, "unsupported member access")))
  | C.GNU_BODY _ ->
     raise (InvalidCUDA (f, l, "gnubody not supported... whatever that is"))
  | C.DESIGNATED _ ->
     raise (InvalidCUDA (f, l, "designated initialization not supported... whatever that is"))
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

and cuda_of_definition da (f, l) d =
  match d with
  | C.DECDEF (t, _, ids) ->
     let mem = mod_of_type false t in
     List.concat (
         List.map (fun (id, t, _, e) ->
             let dims = dims_of_type (f, l) t in
             match e with
             | C.NOTHING -> [CDecl (id, mem, t, dims)]
             | _ -> (match cuda_of_expression da (f, l) e with
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
  let rec is_global t =
    let open C in
    match t with
    | NO_TYPE | VOID | BOOL | CHAR _ | INT _ | BITFIELD _ | FLOAT _
      | DOUBLE _ | COMPLEX_FLOAT | COMPLEX_DOUBLE | COMPLEX_LONG_DOUBLE
      | STRUCT _ | UNION _ | PROTO _ | OLD_PROTO _ | NAMED_TYPE _
      | ENUM _ | BUILTIN_TYPE _
      -> false
    | PTR t | RESTRICT_PTR t
      | ARRAY (t, _)
      | CONST t
      | VOLATILE t
      | GNU_TYPE (_, t)
      | TYPE_LINE (_, _, t) -> is_global t
    | SHARED _ -> false
    | GLOBAL _ -> true
  in
  match t with
  | C.CONST t
    | C.VOLATILE t
    | C.SHARED t
    | C.PTR t
    | C.RESTRICT_PTR t
    | C.GNU_TYPE (_, t)  -> handle_proto (f, l) t
  | C.TYPE_LINE (f, l, t) -> handle_proto (f, l) t
  | C.GLOBAL t -> handle_proto (f, l) t
  | C.PROTO (rt, names, _) ->
     let _ = Printf.printf "NOT GLOBAL\n" in
     (rt, List.map (fun (_, _, (s, t, _, _)) -> (s, t)) names, is_global rt)
(*  | C.OLD_PROTO (_, names, _) ->
     (names, false) *)
  | VOID -> Printf.printf "void?\n";
            raise (InvalidCUDA (f, l, "invalid definition"))
  | _ -> raise (InvalidCUDA (f, l, "invalid definition"))

and cuda_of_topdef da (f, l) d =
  match d with
  | C.FUNDEF ((_, _, (name, t, _, _)), b) ->
     let (rt, params, is_kernel) = handle_proto (f, l) t in
     ([], [rt, name, params, cuda_of_block da (f, l) b, is_kernel])
  | C.OLDFUNDEF ((_, _, (name, t, _, _)), _, b) ->
     let (rt, params, is_kernel) = handle_proto (f, l) t in
     ([], [rt, name, params, cuda_of_block da (f, l) b, is_kernel])
  | C.DECDEF (t, _, ids) ->
     let mem = mod_of_type true t in
     (List.map (fun (id, t, _, _) -> (id, t, mem)) ids, [])
  | C.TYPEDEF _ -> ([], [])
  | C.ONLYTYPEDEF _ -> ([], [])

and cuda_of_file (da: unit -> 'a) name ds : 'a CUDA_Types.cprog =
  List.fold_left
    (fun (g, fn) d ->
      let (g', fn') = cuda_of_topdef da (name, 0) d in
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
  | CVar "__bidx" -> ps fmt "blockIdx.x"
  | CVar "__bidy" -> ps fmt "blockIdy.x"
  | CVar "__bidz" -> ps fmt "blockIdz.x"
  | CVar "__tidx" -> ps fmt "threadIdx.x"
  | CVar "__tidy" -> ps fmt "threadIdy.x"
  | CVar "__tidz" -> ps fmt "threadIdz.x"
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
  let paren_helper binop e1 e2= 
    match edesc e1 with 
    | CL _|CConst _|CParam _ -> 
      (match edesc e2 with 
      | CL _|CConst _|CParam _ -> (fprintf fmt "@[@[%a@]@ %s @[%a@]@]" print_cexpr e1 binop print_cexpr e2)
      | _ ->  (fprintf fmt "@[@[%a@]@ %s @[(%a)@]@]" print_cexpr e1 binop print_cexpr e2)
      )
    | _ -> 
      (match edesc e2 with 
      | CL _|CConst _|CParam _ -> (fprintf fmt "@[@[(%a)@]@ %s @[%a@]@]" print_cexpr e1 binop print_cexpr e2)
      | _ ->  (fprintf fmt "@[@[(%a)@]@ %s @[(%a)@]@]" print_cexpr e1 binop print_cexpr e2)
      )
  in match edesc e with
  | CL v -> print_clval fmt v
  | CConst c -> print_const fmt c
  | CParam p -> print_param fmt p
  | CAdd (e1, e2) -> paren_helper "+" e1 e2
  | CSub (e1, e2) -> paren_helper "-" e1 e2
  | CMul (e1, e2) -> paren_helper "*" e1 e2
  | CDiv (e1, e2) -> paren_helper "/" e1 e2
  | CMod (e1, e2) -> paren_helper "%%" e1 e2
  | CMin args -> fprintf fmt "@[min(%a)@]" print_args args
  | CMax args -> fprintf fmt "@[max(%a)@]" print_args args
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

let print_id_with_bounds fmt (id, lb, ub) = 
   fprintf fmt "@[<v>id:%s@ LB=%a@ UB=%a @]" id print_cexpr lb print_cexpr ub

let print_id_with_bounds_list fmt lst = 
   let pp_sep fmt () = Format.fprintf fmt "%s@ " "\n" in
   Format.pp_print_list ~pp_sep print_id_with_bounds fmt lst
let string_of_mem = function Local -> "__local__"
                           | Global -> "__global__"
                           | Shared -> "__shared__"
                           | Host -> ""

let get_sign si =
  match si with
  | C.NO_SIGN -> ""
  | C.SIGNED -> "signed "
  | C.UNSIGNED -> "unsigned "

let get_size siz =
  match siz with
  | C.NO_SIZE -> ""
  | C.SHORT -> "short "
  | C.LONG -> "long "
  | C.LONG_LONG -> "long long "

let rec string_of_base_type typ id=
   match typ with
   | C.NO_TYPE -> ""
   | C.VOID -> "void"
   | C.BOOL -> "_Bool " ^ id
   | C.CHAR sign -> (get_sign sign) ^ "char " ^ id
   | C.INT (size, sign) -> (get_sign sign) ^ (get_size size) ^ "int " ^ id
   | C.BITFIELD (sign, _) -> (get_sign sign) ^ "int " ^ id
   | C.FLOAT size -> (if size then "long " else "") ^ "float " ^ id
   | C.DOUBLE size -> (if size then "long " else "") ^ "double " ^ id
   | C.COMPLEX_FLOAT -> "float _Complex " ^ id
   | C.COMPLEX_DOUBLE -> "double _Complex " ^ id
   | C.COMPLEX_LONG_DOUBLE -> "long double _Complex " ^ id
   | C.NAMED_TYPE id -> id
   | C.PTR b_type -> string_of_base_type b_type "" ^ "*" ^ id
   | C.VOLATILE b_type -> "volatile "^string_of_base_type b_type id
   | C.CONST b_type -> "const " ^ string_of_base_type b_type id
   | C.ARRAY (b_type, expr) -> string_of_base_type b_type "" ^ id ^ "["  ^ print_expression expr 0 ^ "]"
   | C.STRUCT (name, n_groups) -> "struct " ^ print_fields name n_groups
   | C.UNION (name, n_groups) -> "union " ^ print_fields name n_groups
   | _ -> "other"

and print_fields id (flds) =

   let print_name_group name_group = let (b_type, storage, names) = name_group in
    List.fold_left (fun a name -> (a ^ (print_name name))) "" names
   in
   id ^
   if flds = [] then "" else " { \n" ^ List.fold_left (fun a b -> a ^ print_name_group b ^ ";\n") "" flds ^ "}"


   and print_name (id, typ, attr, exp) = string_of_base_type typ id ^ print_expression exp 1
  
   and print_expression (exp) (lvl : int) =
   let (txt, lvl') = get_operator exp in
   let _ = if lvl > lvl' then "(" else "" in
   (match exp with
       C.NOTHING -> ""
     | C.UNARY (op, exp') -> (match op with
     C.POSINCR | C.POSDECR -> print_expression exp' lvl' ^ txt
        | _ -> txt ^ print_expression exp' lvl')
     | C.BINARY (_, exp1, exp2) -> print_expression exp1 lvl' ^ " " ^ txt ^ " " ^ print_expression exp2 (lvl' + 1)
     (*if (op = SUB) && (lvl <= lvl') then print ")"*)
     | QUESTION (exp1, exp2, exp3) -> print_expression exp1 2 ^ " ? " ^ print_expression exp2 2 ^ " : " ^ print_expression exp3 2;
     | CAST (typ, exp) -> "(" ^ string_of_base_type typ "" ^ ")" ^ print_expression exp 15
     (* | CALL (exp, args) ->
       print_expression exp 16;
       print "(";
       print_comma_exps args;
       print ")"
     | COMMA exps ->
       print_comma_exps exps *)
     | CONSTANT cst -> print_constant cst
     | VARIABLE name -> name
     | EXPR_SIZEOF exp -> "sizeof(" ^ print_expression exp 0 ^ ")"
     | TYPE_SIZEOF typ -> "sizeof(" ^ string_of_base_type typ "" ^ ")"
     | INDEX (exp, idx) -> print_expression exp 16 ^ "[" ^ print_expression idx 0 ^ "]" 
     | MEMBEROF (exp, fld) -> print_expression exp 16 ^ "." ^ fld
     | MEMBEROFPTR (exp, fld) -> print_expression exp 16 ^ "->" ^ fld
     (* | GNU_BODY (decs, stat) ->
       print "(";
       print_statement (BLOCK (decs, stat));
       print ")" *)
     | DESIGNATED (member, exp) -> "." ^ member ^ "=" ^ print_expression exp 16   
     | EXPR_LINE (expr, _, _) ->
       print_expression expr lvl
    | _ -> "other")
   
and print_constant cst = 
      match cst with 
      | C.CONST_INT s -> s
      | C.CONST_FLOAT s -> s
      | C.CONST_CHAR s -> s
      | C.CONST_STRING s -> s
      | _ -> "other"


and get_operator exp =
   match exp with
     NOTHING -> ("", 16)
   | UNARY (op, _) ->
     (match op with
        MINUS -> ("-", 13)
      | PLUS -> ("+", 13)
      | NOT -> ("!", 13)
      | BNOT -> ("~", 13)
      | MEMOF -> ("*", 13)
      | ADDROF -> ("&", 13)
      | PREINCR -> ("++", 13)
      | PREDECR -> ("--", 13)
      | POSINCR -> ("++", 14)
      | POSDECR -> ("--", 14))
   | BINARY (op, _, _) ->
     (match op with
        MUL -> ("*", 12)
      | DIV -> ("/", 12)
      | MOD -> ("%", 12)
      | ADD -> ("+", 11)
      | SUB -> ("-", 11)
      | SHL -> ("<<", 10)
      | SHR -> (">>", 10)
      | LT -> ("<", 9)
      | LE -> ("<=", 9)
      | GT -> (">", 9)
      | GE -> (">=", 9)
      | EQ -> ("==", 8)
      | NE -> ("!=", 8)
      | BAND -> ("&", 7)
      | XOR -> ("^", 6)
      | BOR -> ("|", 5)
      | AND -> ("&&", 4)
      | OR -> ("||", 3)
      | ASSIGN -> ("=", 1)
      | ADD_ASSIGN -> ("+=", 1)
      | SUB_ASSIGN -> ("-=", 1)
      | MUL_ASSIGN -> ("*=", 1)
      | DIV_ASSIGN -> ("/=", 1)
      | MOD_ASSIGN -> ("%=", 1)
      | BAND_ASSIGN -> ("&=", 1)
      | BOR_ASSIGN -> ("|=", 1)
      | XOR_ASSIGN -> ("^=", 1)
      | SHL_ASSIGN -> ("<<=", 1)
      | SHR_ASSIGN -> (">>=", 1))
   | QUESTION _ -> ("", 2)
   | CAST _ -> ("", 13)
   | CALL _ -> ("", 15)
   | COMMA _ -> ("", 0)
   | CONSTANT _ -> ("", 16)
   | VARIABLE _ -> ("", 16)
   | EXPR_SIZEOF _ -> ("", 16)
   | TYPE_SIZEOF _ -> ("", 16)
   | INDEX _ -> ("", 15)
   | MEMBEROF _ -> ("", 15)
   | MEMBEROFPTR _ -> ("", 15)
   | GNU_BODY _ -> ("", 17)
   | DESIGNATED _ -> ("", 15)
   | EXPR_LINE (expr, _, _) -> get_operator expr
(* and print_pointer typ = 
   match typ with
   | C.PTR typ -> print_pointer typ ^ "*"
   | C.RESTRICT_PTR typ -> print_pointer typ ^ "* __restrict "
   | C.CONST typ -> print_pointer typ ^ " const "
   | C.VOLATILE typ -> print_pointer typ ^ " volatile "
   | C.ARRAY (typ, _) -> print_pointer typ *)


let print_for_ending fmt i =
   match i with
   | CAssign(lv, e, _) -> fprintf fmt "@[<h>%a =@ %a@]" print_clval lv print_cexpr e
   | _ -> fprintf fmt "error"

   let rec print_cinstr fmt i =
      match i with
      | CBreak -> ps fmt "break;"
      | CDecl (id, mem, b_type, _) -> (match mem with
                                      | Local -> fprintf fmt "%s;" (string_of_base_type b_type id) 
                                      | _ -> fprintf fmt "%s %s;" (string_of_mem mem) (string_of_base_type b_type id) )
      | CAssign (lv, e, free) ->
         fprintf fmt "@[<h>%a =@ %a;@]" print_clval lv
           (* (if free then "0" else "") *)
           print_cexpr e
      | CIf (l, s1, s2) -> fprintf fmt "@[<v>@[<h>if (%a)@ {@]@ %a@ }@[<h> else@ {@]@ %a @ } @]"
                             print_clogic l
                             print_cblock s1
                             print_cblock s2
      | CWhile (l, s) -> fprintf fmt "@[<v>@[<h>while (%a)@ {@]@ %a @ }@]"
                           print_clogic l
                           print_cblock s
      | CFor (s1, l, s2, s) -> fprintf fmt
                               "@[<v>@[<h>for (%a@ %a;@ %a)@ {@]@,%a @ }@]"
                                 print_for_decl s1
                                 print_clogic l
                                 print_for_ending (List.hd s2)
                                 print_cblock s
      | CReturn e -> fprintf fmt "return %a;" print_cexpr e
      | CSync -> fprintf fmt "__syncthreads()"
    
    and print_for_decl fmt l =
       match l with
       | CDecl(id, mem, b_type, _)::CAssign(lv, e, free)::rest -> fprintf fmt "@[<h>%s = @ %a;@]" (string_of_base_type b_type id) print_cexpr e
       | _ -> fprintf fmt "nothing!"

and print_instr_list fmt l =
  (* List.iter (fun i -> printf "%a@;" print_cinstr i) l *)
  Format.pp_print_list print_cinstr fmt l
and print_cblock fmt (_, b) =
  fprintf fmt "@[<v 4>@ %a@]" print_instr_list b

  and print_ids fmt l =
  let pp_sep fmt () = Format.fprintf fmt "%s@ " "," in
  Format.pp_print_list ~pp_sep print_farg fmt l


and print_farg fmt arg = 
   let (t, name) = arg in fprintf fmt "@[<h> %s@]" (string_of_base_type name t) 

let print_cfunc fmt (rt, name, args, b, kernel) =
  fprintf fmt "@[<v>@[<h>%s%s (%a) {@]@ %a @ }@]"
    (if kernel then "__global__ " else (Printf.printf "not a kernel!\n"; ""))
    name
    print_ids args
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
let submap : unit expr M.t ref = ref M.empty
let add_var_exp x e =
  if Str.string_match (Str.regexp_string "__temp") x 0 ||
       Str.string_match (Str.regexp_string "_reg_") x 0 then
    ((* Printf.printf "Adding %s\n" x; *)
      submap := M.add x (erase_e e) (!submap))
let rec lookup_var x =
  match M.find_opt x (!submap) with
  | None -> mk () (EVar x)
  | Some e -> lookup_exp e
and lookup_exp e =
  match desc e with
  | EVar x -> lookup_var x
  | EAdd (e1, e2) -> mk () (EAdd (lookup_exp e1, lookup_exp e2))
  | ESub (e1, e2) -> mk () (ESub (lookup_exp e1, lookup_exp e2))
  | EMul (e1, e2) -> mk () (EMul (lookup_exp e1, lookup_exp e2))
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

let rec normalize_clval da lv =
  match lv with
  | CVar id -> ([], id)
  | CArr (lv, es) ->
     let (is1, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', an, x) = normalize_cexpr da e in
           (is' @ is, (an, CL (CVar x))::xs))
         ([], [])
         es
     in
     let (is2, x2) = normalize_clval da lv in
     let x = new_var () in
     ((CAssign (CVar x, (da, CL (CArr (CVar x2, xs))), true))::
        (is1 @ is2),
      x)
  | CDeref lv ->
     let (is, x1) = normalize_clval da lv in
     let x = new_var () in
     ((CAssign (CVar x, (da, CL (CDeref (CVar x1))), true))::is, x)
  | CRef lv ->
     let (is, x1) = normalize_clval da lv in
     let x = new_var () in
     ((CAssign (CVar x, (da, CL (CRef (CVar x1))), true))::is, x)
and normalize_clval_write da lv =
  match lv with
  | CVar id -> ([], CVar id)
  | CArr (lv, es) ->
     let (is1, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', an, x) = normalize_cexpr da e in
           (is' @ is, (an, CL (CVar x))::xs))
         ([], [])
         es
     in
     let (is2, x2) = normalize_clval da lv in
     let x = new_var () in
     (is1 @ is2, CArr (CVar x2, xs))
  | CDeref lv ->
     let (is, x1) = normalize_clval da lv in
     (is, CDeref (CVar x1))
  | CRef lv ->
     let (is, x1) = normalize_clval da lv in
     (is, CRef (CVar x1))
and normalize_cexpr da e =
  let normalize_cexpr = normalize_cexpr da in
  let cl an lv = (an, CL lv) in
  let (is, x) =
  match edesc e with
  | CL lv -> normalize_clval da (collapse_arrays lv)
  | CConst _ | CParam _ ->
     let x = new_var () in
     ([CAssign (CVar x, e, true)], x)
  | CAdd (e1, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (mk () (EAdd (mk () (EVar x1), mk () (EVar x2)))) in
     ((CAssign (CVar x, (eann e, CAdd (cl an1 (CVar x1), cl an2 (CVar x2))),
                true))
      ::(is2 @ is1),
      x)
  | CSub (e1, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (mk () (ESub (mk () (EVar x1), mk () (EVar x2)))) in
     ((CAssign (CVar x, (eann e, CSub (cl an1 (CVar x1), cl an2 (CVar x2))),
                true))::(is2 @ is1),
      x)
  | CMul (e1, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     let _ = add_var_exp x (mk () (EMul (mk () (EVar x1), mk () (EVar x2)))) in
     ((CAssign (CVar x, (eann e, CMul (cl an1 (CVar x1), cl an2 (CVar x2))),
                true))::(is2 @ is1),
      x)
  | CDiv (e1, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     ((CAssign (CVar x, (eann e, CDiv (cl an1 (CVar x1), cl an2 (CVar x2))),
                true))::(is2 @ is1),
      x)
  | CMod (e1, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     let x = new_var () in
     ((CAssign (CVar x, (eann e, CMod (cl an1 (CVar x1), cl an2 (CVar x2))),
                true))::(is2 @ is1),
      x)
  | CMin es ->
     let (is, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', an, x) = normalize_cexpr e in
           (is' @ is, (an, CL (CVar x))::xs))
         ([], [])
         es
     in
     let x = new_var () in
     ((CAssign (CVar x, (eann e, CMin xs), true))::is, x)
  | CMax es ->
     let (is, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', an, x) = normalize_cexpr e in
           (is' @ is, (an, CL (CVar x))::xs))
         ([], [])
         es
     in
     let x = new_var () in
     ((CAssign (CVar x, (eann e, CMax xs), true))::is, x)
  | CCall (s, es) ->
     let (is, xs) =
       List.fold_left (fun (is, xs) e ->
           let (is', an, x) = normalize_cexpr e in
           (is' @ is, (an, CL (CVar x))::xs))
         ([], [])
         es
     in
     let x = new_var () in
     ((CAssign (CVar x, (eann e, CCall (s, xs)), true))::is, x)
  in
  (is, eann e, x)
let rec normalize_clogic da l =
  let normalize_clogic = normalize_clogic da in
  let normalize_cexpr = normalize_cexpr da in
  match l with
  | CCmp (e1, c, e2) ->
     let (is1, an1, x1) = normalize_cexpr e1 in
     let (is2, an2, x2) = normalize_cexpr e2 in
     (is2 @ is1, CCmp ((an1, CL (CVar x1)), c, (an2, CL (CVar x2))))
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

let rec normalize_cinstr da i =
  let normalize_cexpr = normalize_cexpr da in
  let normalize_clogic = normalize_clogic da in
  match i with
  | CBreak | CDecl _ -> [i]
  | CAssign (lv, e, b) ->
     let (is1, lv') = normalize_clval_write da (collapse_arrays lv) in
     let (is2, an2, y) = normalize_cexpr e in
     (List.rev is1) @ (List.rev is2) @
       [CAssign (lv', (an2, CL (CVar y)), false)]
  | CIf (l, s1, s2) ->
     let (is1, l) = normalize_clogic l in
     let s1 = normalize_block s1 in
     let s2 = normalize_block s2 in
     (List.rev is1) @ [CIf (l, s1, s2)]
  | CWhile (l, s) ->
     let (is, l) = normalize_clogic l in
     let s = normalize_block s in
     (List.rev is) @ [CWhile (l, add_to_block s is)]
  | CFor (s1, l, s2, s3) ->
     let s1 = normalize_cinstr_list da s1 in
     let (is, l) = normalize_clogic l in
     let s2 = normalize_cinstr_list da s2 in
     let s3 = normalize_block s3 in
     s1 @ ((List.rev is) @ [CFor ([], l, s2, add_to_block s3 is)])
  | CReturn e ->
     let (is, an, x) = normalize_cexpr e in
     [CReturn (an, CL (CVar x))]
  | CSync -> [CSync]
and normalize_cinstr_list da l =
  List.concat (List.map (normalize_cinstr da) l)
and normalize_block (a, b) =
  (a, normalize_cinstr_list a b)

let normalize_func (rt, s, ids, b, is_kernel) =
  (rt, s, ids, normalize_block b, is_kernel)

let normalize_prog (l, funcs) = (l, List.map normalize_func funcs)

let insts ((_, b): 'a cblock) = b
