(* Stefan Muller - 2019 *)

open CUDA_Types
open CUDA
open Types

module I = IMP_Types

type cost = int

type dim3 =
  {x : int;
   y : int;
   z : int}

type array_params =
  { griddim          : dim3;
    blockdim         : dim3;
    blockidx         : dim3 option;
    warp_size        : int;
    global_read_size : int; (* in bits *)
    host_read_size   : int;
    num_shared_banks : int;
    sizeof           : Cabs.base_type -> int; (* in bits *)
    init_thread     : dim3 option;
  }

exception Type of Cabs.base_type

let mk_sizeof ptr_size f t =
  let rec mk in_ptr t =
    let open Cabs in
    match t with
      VOID -> 0
    | NO_TYPE | STRUCT _ | PROTO _ -> 0
    | INT _ | CHAR _ | BITFIELD _ | FLOAT _ | DOUBLE _ -> f t
    | PTR _ | RESTRICT_PTR _ | ARRAY _ when in_ptr -> ptr_size
    | PTR t | RESTRICT_PTR t -> mk true t
    | ARRAY (t, _) | CONST t | SHARED t | GLOBAL t
      | VOLATILE t | GNU_TYPE (_, t)
      | TYPE_LINE (_, _, t) -> mk in_ptr t
    | _ -> failwith "invalid type"
  in mk false t

let sizeof64 =
  mk_sizeof 64
    (let open Cabs in
     function INT (NO_SIZE, _) -> 32
            | INT (SHORT, _) -> 16
            | INT (LONG, _) -> 64
            | INT (LONG_LONG, _) -> 64
            | CHAR _ -> 8
            | FLOAT _ -> 32
            | DOUBLE _ -> 64
            | _ -> failwith "invalid type64")

type unop = UNot | UDeref | URef
type binop = BAdd | BSub | BMul | BDiv | BMod | BAnd | BOr | BCmp

let (++) a b = a + b

type ccost_const =
  | KConst
  | KVar
  | KArrRead of mem * int
  | KUnop of unop
  | KBinop of binop
  | KBreak
  | KDecl of mem
  | KLocAssign
  | KArrAssign of mem * int
  | KIf
  | KDivWarp
  | KThen
  | KElse
  | KWhile
  | KFor
  | KReturn
  | KCall of int (* number of arguments *)
  | KSync

type cmetric = ccost_const -> cost

let cmetric_debug c =
  let s st = Printf.printf "%s\n" st in
  (match c with
   | KConst -> s "const"
   | KVar -> s "var"
   | KArrRead (Shared, n) -> Printf.printf "read (shared, %d)\n" n
   | KArrRead (Global, n) -> Printf.printf "read (global, %d)\n" n
   | KArrRead (Host, n) -> Printf.printf "read (host, %d)\n" n
   | KArrRead (Local, n) -> Printf.printf "read (local, %d)\n" n
   | KUnop _ -> s "unop"
   | KBinop _ -> s "binop"
   | KBreak -> s "break"
   | KDecl _ -> s "decl"
   | KLocAssign -> s "locassign"
   | KArrAssign (Shared, n) -> Printf.printf "write (shared, %d)\n" n
   | KArrAssign (Global, n) -> Printf.printf "write (global, %d)\n" n
   | KArrAssign (Host, n) -> Printf.printf "write (host, %d)\n" n
   | KArrAssign (Local, n) -> Printf.printf "write (local, %d)\n" n
   | KIf -> s "if"
   | KDivWarp -> s "divwarp"
   | KThen -> s "then"
   | KElse -> s "else"
   | KWhile -> s "while"
   | KFor -> s "for"
   | KReturn -> s "return"
   | KCall n -> Printf.printf "call %n\n" n
   | KSync -> s "sync");
  0

let cmetric_wsteps c =
  match c with
  | KArrRead (Global, n) -> 5 * n
  | KArrAssign (Global, n) -> 5 * n
  | KArrRead (Shared, n) -> n
  | KArrAssign (Shared, n) -> n
  | KDivWarp -> 0
  | KElse -> 0
  | KThen -> 0
  | _ -> 1

let cmetric_steps c =
  match c with
  | KArrRead (Global, n) -> n
  | KArrAssign (Global, n) -> n
  | KArrRead (Shared, n) -> n
  | KArrAssign (Shared, n) -> n
  | KDivWarp -> 0
  | KElse -> 0
  | KThen -> 0
  | _ -> 1

let cmetric_divwarps c =
  match c with
  | KDivWarp -> 1
  | _ -> 0

let cmetric_memaccesses c =
  match c with
  | KArrRead (Host, n) -> n
  | KArrAssign (Host, n) -> n
  | KArrRead (Global, n) -> n
  | KArrAssign (Global, n) -> n
  | KArrRead (Shared, n) -> n
  | KArrAssign (Shared, n) -> n
  | _ -> 0

let cmetric_host c =
  match c with
  | KArrRead (Host, n) -> n
  | KArrAssign (Host, n) -> n
  | _ -> 0

let cmetric_global c =
  match c with
  | KArrRead (Global, n) -> n
  | KArrAssign (Global, n) -> n
  | _ -> 0

let cmetric_shared c =
  match c with
  | KArrRead (Shared, n) -> n - 1
  | KArrAssign (Shared, n) -> n - 1
  | _ -> 0

module IdMap = Map.Make(Id)

exception CUDA of string
exception NotNormalized

let mem_of_var vctx v =
  try
    fst (IdMap.find v vctx)
  with Not_found -> Local (* raise (CUDA ("unbound variable " ^ s)) *)
let add_var vctx v m = IdMap.add v m vctx

let costi cost =
  I.ITick cost
let charge cost is =
  (costi cost)::is

let p_none = { pos_file = "";
               pos_line = 0;
               pos_bol = 0;
               pos_char = 0 }

let charge_block cost b =
  { b with I.b_body = (costi cost, p_none)::b.I.b_body }

let block_of_instrs a is =
  { I.b_start_p = p_none;
    I.b_end_p = p_none;
    I.b_body = List.map (fun i -> (i, p_none)) is;
    I.annot = a }

let add_to_block b is =
  { b with I.b_body = b.I.b_body @ (List.map (fun i -> (i, p_none)) is) }

let add_to_block_start is b =
  { b with I.b_body = (List.map (fun i -> (i, p_none)) is) @ b.I.b_body }

let rec imp_of_param ps p =
  let get_dim r d =
    match d with
    | X -> r.x
    | Y -> r.y
    | Z -> r.z
  in
  match p with
  | GridDim d -> ENum (get_dim ps.griddim d)
  | BlockIdx d ->
     (match ps.blockidx, d with
      | (Some bi, _) -> ENum (get_dim bi d)
      | (None, X) -> EVar (CUDA_Config.bidx_var)
      | (None, Y) -> EVar (CUDA_Config.bidy_var)
      | (None, Z) -> EVar (CUDA_Config.bidz_var))
  | BlockDim d -> (*ENum (get_dim ps.blockdim d) *)
     (match d with
      | X -> EVar (CUDA_Config.bdimx_var)
      | Y-> EVar (CUDA_Config.bdimy_var)
      | Z -> EVar (CUDA_Config.bdimz_var))
  (* XXX TODO *)
  | ThreadIdx X -> EVar (CUDA_Config.tidx_var)
  | ThreadIdx Y -> EVar (CUDA_Config.tidy_var)
  | ThreadIdx Z -> EVar (CUDA_Config.tidz_var)
  (*   | ThreadId -> EVar (CUDA_Config.tid_var) *)
  | WarpSize -> ENum (ps.warp_size)
(* | WarpSize -> EVar (CUDA_Config.warpsize_var) *)

let assumps_of_params ps = [] (*
  let tid = EVar (CUDA_Config.tid_var) in
  let tidx = EVar (CUDA_Config.tidx_var) in
  let tidy = EVar (CUDA_Config.tidy_var) in
  let tidz = EVar (CUDA_Config.tidz_var) in
  let (bid, bidx, bidy, bidz) =
    match ps.blockidx with
    | Some bi ->
       let (bidx, bidy, bidz) = (bi.x, bi.y, bi.z) in
       let (bdx, bdy, bdz) = (ps.griddim.x, ps.griddim.y, ps.griddim.z) in
       (ENum (bidx + bidy * bdx + bidz * bdx * bdy),
        ENum bidx,
        ENum bidy,
        ENum bidz)
    | None ->
       (EVar (CUDA_Config.bid_var),
        EVar (CUDA_Config.bidx_var),
        EVar (CUDA_Config.bidy_var),
        EVar (CUDA_Config.bidz_var))
  in
  let warp = ps.warp_size in
  List.map (fun (e1, c, e2) -> I.IAssume (LCmp (e1, c, e2)))
    ([ (tid, Ge, ENum 0); (* XXX TODO *)
      (tid, Lt, ENum warp);
      (tidx, Ge, ENum 0);
      (tidx, Lt, ENum (min warp ps.blockdim.x));
      (tidy, Ge, ENum 0);
      (tidy, Lt, ENum (min (if warp mod ps.blockdim.x = 0 then
                              warp / ps.blockdim.x
                            else warp / ps.blockdim.x + 1)
                         ps.blockdim.y));
      (tidz, Ge, ENum 0);
      (tidz, Lt, ENum (min (if warp mod (ps.blockdim.x * ps.blockdim.y) = 0 then
                              warp / (ps.blockdim.x * ps.blockdim.y)
                            else
                              warp / (ps.blockdim.x * ps.blockdim.y) + 1)
                         ps.blockdim.z))
    ] @
    (match ps.blockidx with
     | None ->
        [ (bid, Ge, ENum 0);
          (bid, Lt, ENum (ps.griddim.x * ps.griddim.y * ps.griddim.z));
          (bidx, Ge, ENum 0);
          (bidx, Lt, ENum ps.griddim.x);
          (bidy, Ge, ENum 0);
          (bidy, Lt, ENum ps.griddim.y);
          (bidz, Ge, ENum 0);
          (tidz, Ge, ENum ps.griddim.z)
        ]
     | Some _ -> [])) *)

let var_is_free v =
  Str.string_match (Str.regexp_string "__temp") v 0

let charge_var m v l =
  if var_is_free v then l
  else charge (m KVar) l

let rec imp_of_cexpr da vctx p m e =
  let mka = mk (ann e) in
  match edesc e with
  | CL (CVar id) -> (charge (if var_is_free id then 0 else m KVar) [],
                     mka (EVar id))
  | CL (CArr (CVar id, inds)) ->
     let (mem, size, dims) = IdMap.find id vctx in
     (* let _ = Printf.printf "dims(%s) = %d\n" id ((List.length dims) + 1) in
     let _ = Printf.printf "%d indices\n" (List.length inds) in *)
     let (vc, ind) =
       try
         List.fold_right2
           (fun ind dim (vc, e) ->
             match edesc ind with
             | CL (CVar id) ->
                let mk = mk (ann ind) in
                ((if var_is_free id then vc else vc + (m KVar)),
                 mk (EAdd (mk (EVar id), mk (EMul (e, mk (ENum dim))))))
             | _ -> raise NotNormalized)
           inds
           ((List.rev dims) @ [1])
           (0, mk (da ()) (ENum 0))
       with Invalid_argument _ -> failwith "mis-dimensioned array"
     in
     let ind_var = new_var () in
     let cvar = new_var () in
     let rvar = new_var () in
     let c = match mem with
       | Shared -> [I.ITickConflicts (ind_var, ann ind, size, true)]
       | Host -> [I.ITickMemReads (ind_var, ann ind, size, true, true)]
       | Global -> [I.ITickMemReads (ind_var, ann ind, size, false, true)]
       | Local -> [I.ITick (m KVar)]
     in
     (* As an optimization, could just not emit this assignment
      * if ind is a variable *)
     let _ = add_var_exp ind_var ind in
     ((costi vc)::I.IAssign (ind_var, ind)::c @
           [I.ICallUninterp (rvar, id, [ind_var])], mka (EVar rvar))
  | CL (CDeref (CVar id)) ->
     let (mem, _, _) = IdMap.find id vctx in
     let c = m (KArrRead (mem,
                          (match mem with
                           | Shared -> p.warp_size
                           | _ -> 1)))
     in
     (charge (c ++ (m (KUnop UDeref))) [], mka (EVar id))
  | CL (CRef (CVar id)) ->
     (charge ((m KVar) ++ (m (KUnop URef))) [], mka (EVar id))
  | CConst (CInt n) -> (charge (m KConst) [], mka (ENum n))
  | CConst _ -> (charge (m KConst) [], mka (ENum 0))
  | CParam e -> (charge (m KConst) [], mka (imp_of_param p e))
  | CAdd ((a1, CL (CVar x1)), (a2, CL (CVar x2))) ->
     (charge (m (KBinop BAdd))
        (charge_var m x1 (charge_var m x2 [])),
      mka (EAdd (mk a1 (EVar x1), mk a2 (EVar x2))))
  | CSub ((a1, CL (CVar x1)), (a2, CL (CVar x2))) ->
     (charge (m (KBinop BSub))
        (charge_var m x1 (charge_var m x2 [])),
      mka (ESub (mk a1 (EVar x1), mk a2 (EVar x2))))
  | CMul ((a1, CL (CVar x1)), (a2, CL (CVar x2))) ->
          (charge (m (KBinop BMul))
        (charge_var m x1 (charge_var m x2 [])),
      mka (EMul (mk a1 (EVar x1), mk a2 (EVar x2))))
  | CDiv ((_, CL (CVar x1)), (_, CL (CVar x2))) ->
     let rvar = new_var () in
     (charge (m (KBinop BDiv))
        (charge_var m x1
           (charge_var m x2 [I.ICallUninterp (rvar, "__div", [x1; x2])])),
      (*
        [I.IAssign (svar, EVar x1);
         I.IWhile (LCmp (EVar svar, Ge, ENum 0),
                   block_of_instrs [I.IAssign (svar, ESub (EVar svar, EVar x2));
                                    I.IAssign (rvar, EAdd (EVar rvar, ENum 1))])],
       *)
      (* [I.IAssume (LCmp (EMul (EVar rvar, EVar x2), Eq, EVar x1))], *)
      mka (EVar rvar))
  | CMod ((_, CL (CVar x1)), (_, CL (CVar x2))) ->
     let rvar = new_var () in
     (charge (m (KBinop BMod))
        (charge_var m x1
           (charge_var m x2
              [I.ICallUninterp (rvar, "__mod", [x1; x2]);
               (* These are maybe useful sometimes, but make Presburger
                * really slow. *)
              (*
               I.IAssume (LAnd (LCmp (EVar rvar, Ge, ENum 0),
                                LCmp (EVar rvar, Lt, EVar x2)))*)])),
      mka (EVar rvar))
  | CMin es ->
     let rvar = new_var () in
     (charge (m (KCall (List.length es)))
        (List.concat
           (List.map
              (function
               | (a, CL (CVar x)) ->
                  charge_var m x
                    [I.IAssume (LCmp (mka (EVar rvar), Le, mk a (EVar x)))]
               | _ -> raise NotNormalized)
              es
           )
        ),
      mka (EVar rvar)
     )
  | CMax es ->
     let rvar = new_var () in
     (charge (m (KCall (List.length es)))
        (List.concat
           (List.map
              (function
               | (a, CL (CVar x)) ->
                  charge_var m x
                    [I.IAssume (LCmp (mka (EVar rvar), Ge, mk a (EVar x)))]
               | _ -> raise NotNormalized)
              es
           )
        ),
      mka (EVar rvar)
     )
  | _ -> raise NotNormalized

let rec imp_of_clogic da vctx p m l =
  let imp_of_cexpr = imp_of_cexpr da in
  let imp_of_clogic = imp_of_clogic da in
  match l with
  | CCmp (e1, c, e2) ->
     let (is1, e1) = imp_of_cexpr vctx p m e1 in
     let (is2, e2) = imp_of_cexpr vctx p m e2 in
     (charge (m (KBinop BCmp)) (is1 @ is2), LCmp (e1, c, e2))
  | CAnd (l1, l2) ->
     let (is1, l1) = imp_of_clogic vctx p m l1 in
     let (is2, l2) = imp_of_clogic vctx p m l2 in
     (charge (m (KBinop BAnd)) (is1 @ is2), LAnd (l1, l2))
  | COr (l1, l2) ->
     let (is1, l1) = imp_of_clogic vctx p m l1 in
     let (is2, l2) = imp_of_clogic vctx p m l2 in
     (charge (m (KBinop BOr)) (is1 @ is2), LOr (l1, l2))
  | CNot l ->
     let (is, l) = imp_of_clogic vctx p m l in
     (charge (m (KUnop UNot)) is, LNot l)

let block_is_empty (_, b) = b = []
     
let rec imp_of_cinstr a vctx p m i =
  let imp_of_cinstr = imp_of_cinstr a in
  let imp_of_cexpr = imp_of_cexpr a in
  let imp_of_clogic = imp_of_clogic a in
  match i with
  | CBreak -> ([costi (m KBreak); I.IBreak], vctx)
  | CDecl (id, mem, t, dims) -> ([costi (m (KDecl mem))],
                                 add_var vctx id (mem, p.sizeof t, dims))
  | CAssign (lv, e, free) ->
     (match (e, lv) with
        ((_, CCall (f, args)), CVar r) ->
         let (arg_c, arg_ids) =
           List.fold_left (fun (c, ids) arg ->
               match edesc arg with (CL (CVar x)) ->
                               ((if var_is_free x then c else c + (m KVar)),
                                x::ids)
                            | _ -> raise NotNormalized)
             (0, [])
             args
       in
       ([costi ((m (KCall (List.length args))) ++
                  arg_c ++ (if free then 0 else (m KLocAssign)));
         I.ICallUninterp (r, f, arg_ids)],
        vctx)
      | _ ->
     (let (is, e) = imp_of_cexpr vctx p m e in
      match lv with
      | CVar id ->
         ((match desc e with
           | EVar t -> add_var_exp t (mk () (EVar id))
           | _ -> ());
          (is @ [costi (if free then 0 else m KLocAssign);
                 I.IAssign (id, e)], vctx))
      | CArr (CVar id, inds) ->
         let (mem, size, dims) = IdMap.find id vctx in
         let (vc, ind) =
           try
             List.fold_right2
               (fun ind dim (vc, e) ->
                 match edesc ind with
                 | CL (CVar id) ->
                    let mk = mk (ann ind) in
                    ((if var_is_free id then vc else vc + (m KVar)),
                     mk (EAdd (mk (EVar id), mk (EMul (e, mk (ENum dim))))))
                 | _ -> raise NotNormalized)
               inds
               ((List.rev dims) @ [1])
               (0, mk (a ()) (ENum 0))
           with Invalid_argument _ -> failwith "mis-dimensioned array"
         in
         let ind_var = new_var () in
         let cvar = new_var () in
         let c = match mem with
           | Shared -> [I.ITickConflicts (ind_var, ann ind, size, false)]
           | Host -> [I.ITickMemReads (ind_var, ann ind, size, true, false)]
           | Global -> [I.ITickMemReads (ind_var, ann ind, size, false, false)]
           | Local -> [I.ITick (m KVar)]
         in
         (is @ ((costi vc)::(I.IAssign (ind_var, ind))::c), vctx)
      | CDeref (CVar id)->
         let (mem, _, _) = IdMap.find id vctx in
         let c = m (KArrAssign (mem,
                                (match mem with
                                 | Shared -> p.warp_size
                                 | _ -> 1)))
         in
         ([costi ((m (KUnop UDeref)) ++ c)], vctx)
      | CRef _ -> ([costi ((m (KUnop URef)) ++ (m KVar))], vctx)
      | _ -> raise NotNormalized))

  (* comment if counting divwarps without else branch *)

  | CIf (l, s, b) when block_is_empty b ->
     let (is, l) = imp_of_clogic vctx p m l in
     let (s, _) = imp_of_block vctx p m s in
     (is @ [costi (m KIf); I.IIfNoElse (l, charge_block (m KThen) s)], vctx)
  | CIf (l, b, s) when block_is_empty b ->
     let (is, l) = imp_of_clogic vctx p m l in
     let (s, _) = imp_of_block vctx p m s in
     (is @ [costi (m KIf); I.IIfNoElse (LNot l,
                                        charge_block (m KElse) s)], vctx)

  | CIf (l, s1, s2) ->
     let (is, l) = imp_of_clogic vctx p m l in
     let (s1, _) = imp_of_block vctx p m s1 in
     let (s2, _) = imp_of_block vctx p m s2 in
     (is @ [costi (m KIf); I.IIf (l, charge_block (m KThen) s1,
                                  charge_block (m KElse) s2)], vctx)
  | CWhile (l, s) ->
     let (is, l) = imp_of_clogic vctx p m l in
     let (s, _) = imp_of_block vctx p m s in
     (is @ [costi (m KIf); I.IWhile (l, charge_block ((m KThen) ++ (m KIf))
                                         (add_to_block s (is @ [costi (m KIf)])));
            costi (m KElse)],
      vctx)
  | CFor ([], l, s2, s3) ->
     let (is, l) = imp_of_clogic vctx p m l in
     let (s2, _) = imp_of_cinstr_list vctx a p m s2 in
     let (s3, _) = imp_of_block vctx p m s3 in
     (is @ [costi (m KIf); I.IWhile (l, charge_block ((m KThen) ++ (m KIf))
                                          (block_of_instrs s3.I.annot
                                             ((List.map fst s3.I.b_body) @
                                                (List.map fst s2.I.b_body) @
                                                  is)));
            costi (m KElse)],
      vctx)
  | CReturn (_, CL (CVar id)) ->
     ([costi (m KReturn); I.IReturn [id]], vctx)
  | CSync ->
     ([costi (m KSync)], vctx)
  | _ -> raise NotNormalized
and imp_of_cinstr_list vctx a p m l =
  let (is, vctx') =
    List.fold_left (fun (is, vctx) i ->
        let (is', vctx') = imp_of_cinstr a vctx p m i in
        (is @ is', vctx'))
      ([], vctx)
      l
  in
  (block_of_instrs (a ()) is, vctx')
and imp_of_block vctx p m ((a, b) : 'a cblock) : 'a I.block * 'b =
  imp_of_cinstr_list vctx (fun () -> a) p m b
  

let dom m =
  List.fold_left (fun s (k, _) -> IdSet.add k s)
    IdSet.empty
    (IdMap.bindings m)

let imp_of_func vctx p m ((_, s, ids, b, is_kernel) : 'a cfunc) : 'a I.func  =
  let vctx = List.fold_left
               (fun vctx (v, t) -> add_var vctx v (Global, p.sizeof t, []))
               vctx
               ids
  in
  let (b, vctx') = imp_of_block vctx p m b in
  let b = add_to_block_start (assumps_of_params p) b in
  let fun_ids = IdSet.elements (IdSet.diff (dom vctx') (dom vctx))
  in
  { fun_name = s;
    fun_vars = fun_ids;
    fun_focus = [];
    fun_body = b;
    fun_start_p = p_none;
    fun_end_p = p_none;
    fun_args = List.map fst ids
  }

let imp_of_prog p m ((globals, funcs) : 'a CUDA_Types.cprog) :
      'b * ('a I.func list) =
  let (global, funcs) = normalize_prog (globals, funcs) in
  (* let _ = print_cprog Format.std_formatter (global, funcs) in *)
  (List.map (fun (x, _, _) -> x) globals,
   let vctx = List.fold_left
                (fun vctx (v, t, m) -> add_var vctx v (m, p.sizeof t, []))
               IdMap.empty
               globals
   in
   List.map (fun f -> IMP_Simpl.simpl_func (imp_of_func vctx p m f)) funcs)
