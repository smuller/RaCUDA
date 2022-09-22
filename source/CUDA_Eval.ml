(* Stefan Muller - 2019 *)

open Types
open CUDA_Types
open CUDA_Cost
open CUDA

type thread_idx = dim3

let real_tid_of_idx params d =
  d.x + d.y * params.blockdim.x + d.z * params.blockdim.x * params.blockdim.y
let tid_of_idx params d =
  let tid = real_tid_of_idx params d
  in
  match params.init_thread with
  | Some tx -> tid - (real_tid_of_idx params tx)
  | None -> tid

module M = Map.Make(Id)

type lmem = const option array M.t

exception CannotEval of string

let mem_of_var vctx v =
  try
    fst (IdMap.find v vctx)
  with Not_found -> Local (* raise (CUDA ("unbound variable " ^ s)) *)

let eval_param params threads p : const option list =
  match p with
  | GridDim X -> List.map (fun _ -> Some (CInt (params.griddim.x))) threads
  | GridDim Y -> List.map (fun _ -> Some (CInt (params.griddim.y))) threads
  | GridDim Z -> List.map (fun _ -> Some (CInt (params.griddim.z))) threads
  | BlockIdx d ->
     (match (params.blockidx, d) with
      | (Some bi, X) -> List.map (fun _ -> Some (CInt (bi.x))) threads
      | (Some bi, Y) -> List.map (fun _ -> Some (CInt (bi.y))) threads
      | (Some bi, Z) -> List.map (fun _ -> Some (CInt (bi.z))) threads
      | (None, _) -> raise (CannotEval "block index required for evaluation"))
  | BlockDim X -> List.map (fun _ -> Some (CInt (params.blockdim.x))) threads
  | BlockDim Y -> List.map (fun _ -> Some (CInt (params.blockdim.y))) threads
  | BlockDim Z -> List.map (fun _ -> Some (CInt (params.blockdim.z))) threads
  | ThreadIdx X -> List.map (fun tx -> Some (CInt (tx.x))) threads
  | ThreadIdx Y -> List.map (fun tx -> Some (CInt (tx.y))) threads
  | ThreadIdx Z -> List.map (fun tx -> Some (CInt (tx.z))) threads
  | WarpSize -> List.map (fun _ -> Some (CInt (params.warp_size))) threads

let rec eval_clval params m vctx lm heap read (threads: dim3 list) lv thr_ref
        : const option list * cost =
  match lv with
  | CVar id ->
     ((try
        let va = M.find id lm in
        (List.map (fun tx -> va.(tid_of_idx params tx)) threads)
      with Not_found -> List.map (fun _ -> None) threads)
     ,
       if thr_ref then
         (let (mem, _, _) = IdMap.find id vctx in
          if read then
            m (KArrRead (mem,
                         (match mem with
                          | Shared -> params.warp_size
                          | _ -> 1)))
          else
            m (KArrAssign (mem,
                           (match mem with
                            | Shared -> params.warp_size
                            | _ -> 1))))
       else
         if read then m KVar else m KLocAssign)
  | CArr (CVar id, es) ->
     (* let _ = Printf.printf "array\n" in *)
     let (mem, type_size, dims) = IdMap.find id vctx in
     let (inds, ecost) =
       try
         List.fold_right2
           (fun ind dim (es, c) ->
             let (es', c') = eval_cexpr params m vctx lm heap threads ind in
             (List.map2 (fun pres nres ->
                  match nres with
                  | Some (CInt n) -> n + pres * dim
                  | _ -> raise (CannotEval "type error - index"))
                es
                es',
              c ++ c'))
           (List.rev es)
           ((List.rev dims) @ [1])
           (List.map (fun _ -> 0) threads, 0)
       with Invalid_argument _ -> raise (CannotEval "mis-dimensioned array")
     in
     let n =
       (match mem with
        | Global | Host ->
           let read_size = (match mem with
                            | Global -> params.global_read_size
                            | _ -> params.host_read_size)
           in
           let reads =
             List.fold_left
               (fun reads i ->
                 if
                   List.exists
                     (fun i' -> (* Printf.printf "%d, %d\n" i i'; *)
                       (i * type_size >= i' * type_size) &&
                         (i * type_size < i' * type_size + read_size))
                     reads
                 then
                   reads
                 else
                   (((i * type_size / read_size) * read_size) / type_size)::reads)
               []
               inds
           in
           let reads = List.length reads in
           (* let _ = Printf.printf "%d reads\n" reads in *)
           reads
        | Shared ->
           let bs = params.num_shared_banks in
           let accs = Array.make bs 0 in
           let _ = List.iter
                     (fun i -> let b = i * type_size / 32 in
                               accs.(b mod bs) <- accs.(b mod bs) + 1)
                     (List.sort_uniq Int.compare inds)
           in
           let conflicts = Array.fold_left max 0 accs in
           (* let _ = Printf.printf "%d conflicts\n" conflicts in *)
           conflicts
        | Local -> raise (CannotEval "array should not be local"))
     in
     let vals =
       try
         let va = M.find id lm in
         List.map2
           (fun tx i ->
             match va.(tid_of_idx params tx) with
             | Some (CPtr (addr, offset)) ->
                if read then
                  (try
                     Some (Heap.lookup (addr, offset + i) heap)
                   with
                   | Not_found -> None
                   | Invalid_argument _ ->
                      raise (CannotEval "array access out of bounds"))
                else
                  Some (CPtr (addr, offset + i))
             | None -> None
             | _ -> raise (CannotEval "not an array"))
           threads
           inds
       with Not_found -> List.map (fun _ -> None) threads
     in
         (vals,
          ecost ++ (if read then m (KArrRead (mem, n))
                    else m (KArrAssign (mem, n))))
  | CArr _ -> raise (CannotEval "invalid array access")
  | CDeref lv ->
     let (_, cost) = eval_clval params m vctx lm heap read threads lv true in
     (List.map (fun _ -> None) threads, cost ++ (m (KUnop UDeref)))
  | CRef lv ->
     let (_, cost) = eval_clval params m vctx lm heap read threads lv thr_ref in
     (List.map (fun _ -> None) threads, cost ++ (m (KUnop URef)))

and eval_cexpr params m vctx lm heap (threads: dim3 list) e :
      const option list * cost =
  match desc e with
  | CL lv ->
     eval_clval params m vctx lm heap true threads (collapse_arrays lv) false
  | CConst c -> (List.map (fun _ -> Some c) threads, m KConst)
  | CParam p -> (eval_param params threads p, m KConst)
  | CAdd (e1, e2) ->
     let (es1, c1) = eval_cexpr params m vctx lm heap threads e1 in
     let (es2, c2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun e1 e2 ->
          match (e1, e2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some (CInt n1), Some (CInt n2)) -> Some (CInt (n1 + n2))
          | (Some (CFloat n1), Some (CFloat n2)) -> Some (CFloat (n1 +. n2))
          | _ -> raise (CannotEval "type error - add"))
        es1
        es2,
      c1 ++ c2 ++ (m (KBinop BAdd)))
  | CSub (e1, e2) ->
     let (es1, c1) = eval_cexpr params m vctx lm heap threads e1 in
     let (es2, c2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun e1 e2 ->
          match (e1, e2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some (CInt n1), Some (CInt n2)) -> Some (CInt (n1 - n2))
          | (Some (CFloat n1), Some (CFloat n2)) -> Some (CFloat (n1 -. n2))
          | _ -> raise (CannotEval "type error - sub"))
        es1
        es2,
      c1 ++ c2 ++ (m (KBinop BSub)))
  | CMul (e1, e2) ->
     let (es1, c1) = eval_cexpr params m vctx lm heap threads e1 in
     let (es2, c2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun e1 e2 ->
          match (e1, e2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some (CInt n1), Some (CInt n2)) -> Some (CInt (n1 * n2))
          | (Some (CFloat n1), Some (CFloat n2)) -> Some (CFloat (n1 *. n2))
          | _ -> raise (CannotEval "type error - mul"))
        es1
        es2,
      c1 ++ c2 ++ (m (KBinop BMul)))
  | CDiv (e1, e2) ->
     let (es1, c1) = eval_cexpr params m vctx lm heap threads e1 in
     let (es2, c2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun e1 e2 ->
          match (e1, e2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some (CInt n1), Some (CInt n2)) -> Some (CInt (n1 / n2))
          | (Some (CFloat n1), Some (CFloat n2)) -> Some (CFloat (n1 /. n2))
          | _ -> raise (CannotEval "type error - div"))
        es1
        es2,
      c1 ++ c2 ++ (m (KBinop BDiv)))
  | CMod (e1, e2) ->
     let (es1, c1) = eval_cexpr params m vctx lm heap threads e1 in
     let (es2, c2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun e1 e2 ->
          match (e1, e2) with
          | (None, _) -> None
          | (_, None) -> None
          | (Some (CInt n1), Some (CInt n2)) -> Some (CInt (n1 mod n2))
          | _ -> raise (CannotEval "type error - mod"))
        es1
        es2,
      c1 ++ c2 ++ (m (KBinop BMod)))
  | CCall (s, es) ->
     let c =
       List.fold_left (fun c e ->
           let (_, c') = eval_cexpr params m vctx lm heap threads e in
           c ++ c')
         0
         es
     in
     (List.map (fun _ -> None) threads, c ++ (m (KCall (List.length es))))

and eval_clogic params m vctx lm heap threads l : bool list * cost =
  match l with
  | CCmp (e1, cmp, e2) ->
     let (cs1, cost1) = eval_cexpr params m vctx lm heap threads e1 in
     let (cs2, cost2) = eval_cexpr params m vctx lm heap threads e2 in
     (List.map2 (fun c1 c2 ->
          match (c1, c2) with
          | (None, _) -> raise (CannotEval "cannot branch on abstract values")
          | (_, None) -> raise (CannotEval "cannot branch on abstract values")
          | (Some (CInt n1), Some (CInt n2)) ->
             (match cmp with Le -> (<=) | Lt -> (<) | Eq -> (=) | Ne -> (<>)
                             | Ge -> (>=) | Gt -> (>))
               n1 n2
          | (Some (CFloat n1), Some (CFloat n2)) ->
             (match cmp with Le -> (<=) | Lt -> (<) | Eq -> (=) | Ne -> (<>)
                             | Ge -> (>=) | Gt -> (>))
               n1 n2
          | _ -> raise (CannotEval "type error"))
        cs1 cs2,
      cost1 ++ cost2 ++ (m (KBinop BCmp)))
  | CAnd (l1, l2) ->
     let (b1, cost1) = eval_clogic params m vctx lm heap threads l1 in
     let (b2, cost2) = eval_clogic params m vctx lm heap threads l2 in
     (List.map2 (&&) b1 b2, cost1 ++ cost2 ++ (m (KBinop BAnd)))
  | COr (l1, l2) ->
     let (b1, cost1) = eval_clogic params m vctx lm heap threads l1 in
     let (b2, cost2) = eval_clogic params m vctx lm heap threads l2 in
     (List.map2 (||) b1 b2, cost1 ++ cost2 ++ (m (KBinop BOr)))
  | CNot l ->
     let (b, cost) = eval_clogic params m vctx lm heap threads l in
     (List.map (not) b, cost ++ (m (KUnop UNot)))

and eval_cinstr params m vctx lm heap cost (stack: (dim3 list * 'a cinstr list) list)
    : const option list option * cost =
  (* let _ = Printf.printf "cost: %d\n" cost in *)
  match stack with
  | [] -> (None, cost)
  | (_, [])::s -> eval_cinstr params m vctx lm heap cost s
  | ([], _)::s -> eval_cinstr params m vctx lm heap cost s
  | (_, CBreak::_)::s -> eval_cinstr params m vctx lm heap
                           (cost ++ (m KBreak)) s
  | (ts, (CDecl (id, mem, t, dims))::r)::s ->
     eval_cinstr params m (M.add id (mem, params.sizeof t, dims) vctx)
       lm heap
       (cost ++ (m (KDecl mem)))
       ((ts, r)::s)
  | (ts, (CSync::r))::s ->
     eval_cinstr params m vctx lm heap (cost ++ (m KSync)) ((ts, r)::s)
  | (ts, (CAssign (lv, e, free))::r)::s ->
     (assert (not free);
      let (vals, lvc) = eval_clval params m vctx lm heap false ts
                       (collapse_arrays lv)
                       false
      in
      let (es, ec) = eval_cexpr params m vctx lm heap ts e in
      let lm' =
        match lv with
        | CVar id ->
           let arr = match M.find_opt id lm with
             | Some arr -> arr
             | None -> Array.make (params.warp_size) (Some (CInt 0))
           in
           List.iter2 (fun tx e -> arr.(tid_of_idx params tx) <- e) ts es;
           M.add id arr lm
        | _ -> lm
      in
      let heap' =
        List.fold_left2 (fun heap p v ->
            match (p, v) with
            | (Some (CPtr (addr, offset)), Some v) ->
               Heap.write (addr, offset) v heap
            | _ -> heap)
          heap
          vals
          es
      in
      eval_cinstr params m vctx lm' heap' (cost ++ lvc ++ ec) ((ts, r)::s))
  | (ts, (CIf (l, b1, b2))::r)::s ->
     let (cond, ccost) = eval_clogic params m vctx lm heap ts l in
     let (tthreads, fthreads) =
       List.fold_left2 (fun (t, f) cond tx ->
                              if cond then (tx::t, f)
                              else (t, tx::f))
         ([], [])
         cond
         ts
     in
     (match (tthreads, fthreads, insts b1, insts b2) with
      | ([], _, _, x::t) -> eval_cinstr params m vctx lm heap
                              (cost ++ ccost ++ (m KIf) ++ (m KElse))
                              ((ts, insts b2)::(ts, r)::s)
      | ([], _, _, [])
        | (_, [], [], _) -> eval_cinstr params m vctx lm heap
                              (cost ++ ccost ++ (m KIf))
                              ((ts, r)::s)
      | (_, [], x::t, _) -> eval_cinstr params m vctx lm heap
                              (cost ++ ccost ++ (m KIf) ++ (m KThen))
                              ((ts, insts b1)::(ts, r)::s)
                              (* Uncomment if not charging for skips
      | (tt, ft, [], _) -> eval_cinstr params m vctx lm
                             (cost ++ ccost + (m KIf) + (m KElse))
                             ((ft, b2)::(ts, r)::s)
      | (tt, ft, _, []) -> eval_cinstr params m vctx lm
                             (cost ++ ccost + (m KIf) + (m KThen))
                             ((tt, b1)::(ts, r)::s)
                               *)
      | (tt, ft, _, _) ->
         eval_cinstr params m vctx lm heap
           (cost ++ ccost ++ (m KDivWarp) ++ (m KIf) ++ (m KElse) ++ (m KThen))
           ((tt, insts b1)::(ft, insts b2)::(ts, r)::s)
     )
  | (ts, (CWhile (l, (a, b)))::r)::s ->
     ((*Printf.printf "while with %d threads\n" (List.length ts);*)
     eval_cinstr params m vctx lm heap cost
       ((ts, (CIf (l, (a, b @ [CWhile (l, (a, b))]), (a, [])))::r)::s))
  | (ts, (CFor (b1, l, b2, (a, b3)))::r)::s ->
     eval_cinstr params m vctx lm heap cost
       ((ts, b1 @ [CWhile (l, (a, b3 @ b2))] @ r)::s)
  | (ts, (CReturn e)::r)::_ ->
     let (es, c) = eval_cexpr params m vctx lm heap ts e in
     (Some es, c ++ cost ++ (m KReturn))

let eval_cfunc params lm h m threads (_, _, ids, b, _) =
  let vctx = List.fold_left
               (fun vctx (v, t) -> add_var vctx v (Global, params.sizeof t, []))
               M.empty
               ids
  in
  eval_cinstr params m vctx lm h 0 [(threads, insts b)]
