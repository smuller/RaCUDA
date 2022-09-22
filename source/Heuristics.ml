(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

open Types
open Graph
open Polynom
open Focus_Builder
open Focus

module PSet = Set.Make(struct
  type t = Poly.t
  let compare = Poly.compare
end)

module ISet = Set.Make(struct
  type t = int
  let compare = compare
end)

let rec dedup_sorted l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x::y::t -> if x <> y then x::(dedup_sorted (y::t))
               else x::(dedup_sorted t)

let pset_map f t =
  PSet.fold (fun x xs -> PSet.add (f x) xs) t PSet.empty

let pset_of_logic log =
  let cmp e1 c e2 =
    let p1 = Poly.of_expr e1
    and p2 = Poly.of_expr e2 in
    let one = Poly.const 1. in
    match c with
    | Le -> [[Poly.sub p2 p1]]
    | Lt -> [[Poly.sub (Poly.sub p2 p1) one]]
    | Ge -> [[Poly.sub p1 p2]]
    | Gt -> [[Poly.sub (Poly.sub p1 p2) one]]
    | _ -> [[]]
  in
  match DNF.of_logic cmp log with
  | [l] -> List.fold_left
    (fun ps pol -> PSet.add pol ps)
    PSet.empty l
  | _ -> PSet.empty

type loop = {
  l_head: int;
  mutable l_body: ISet.t;
  mutable l_chld: loop list;
  mutable l_base: PSet.t;
  mutable l_incr: PSet.t;
}

let add_focus ?(degree=1) ai_results ai_get_nonneg ai_is_nonneg gfunc =

  (* While following the rpo in the function
     graph, we collect all the loops we can
     find.  Loops are represented as sets of
     integers.

     For each loop, we collect the "continue"
     conditions.  Each of them, combined with
     a maximum increment in the loop will give
     two base functions of interest.
  *)
  let ai_results = Hashtbl.find ai_results gfunc.fun_name in
  let edges = gfunc.fun_body.g_edges in
  let nnodes = Array.length edges in

  let preds = Array.make nnodes [] in
  let _ =
    Array.iteri (fun pred ->
      List.iter (fun (_, succ) ->
        preds.(succ) <- pred :: preds.(succ))
    ) edges;
    Array.map (fun l -> dedup_sorted (List.sort compare l)) preds
  in

  let mkloop head = {
    l_head = head;
    l_body = ISet.empty;
    l_chld = [];
    l_base = PSet.empty;
    l_incr = PSet.empty;
  } in

  let root = mkloop (-1) in
  let loopinfo = Array.make nnodes root in

  let _ =
    let rec collect l n =
      if loopinfo.(n) == l || n < l.l_head then
        (* Already visited, or clearly out of
           the loop. *)
        ISet.empty
      else begin
        loopinfo.(n) <- l;
        List.fold_left
          (fun body pred ->
            ISet.union body (collect l pred))
          (ISet.singleton n)
          preds.(n)
      end
    in
    let ishead node = List.exists
      (fun pred -> pred >= node)
      preds.(node)
    in
    for node = 0 to nnodes-1 do
      if ishead node then begin
        let ldad = loopinfo.(node) in
        let lme = mkloop node in
        ldad.l_chld <- lme :: ldad.l_chld;
        lme.l_body <- collect lme node;
      end
    done in

  let iterloops ?pre f =
    let rec go f l =
      begin match pre with
      | None -> ()
      | Some pre -> pre l
      end;
      List.iter (go f) l.l_chld;
      f l in
    go f root in

  iterloops begin fun l ->
    l.l_chld <- List.sort
      (fun a b -> compare b.l_head a.l_head)
      l.l_chld;
  end;

  (* For each base in each loop, figure out its possible
     decrements in the loop body.
       - If only one decrement is found, add it to the
         base to figure some candidate potential.
       - If none or multiple are found, form a candidate
         by adding one to the base.
     Add the rewrite m0(cand) >= cand-base + m0(base)
     Add the rewrite m0(cand) >= m0(base) (to weaken on gopan style examples)

     For each base of the loop and children loops,
     check if it can grow in the current loop.
     Check all assertions in the path to the modification,
     all of those that are changed in the loop are added
     in the base set to process.

     For each assignment changing a base, create and add
     the new base resulting from execution of the assignment.
     Generate the rewrite function m0(base) >= m0(base').
     If x = y is the assignment, add x - y if there is
     a base with -x, and y - x if there is a base with x.
  *)

  let classify node p = function
    (* Classify an action as an increment,
       a decrement, a reset, a no-op, or
       something else.
    *)
    | AAssign (v, e, _) when desc !e = ERandom ->
      if Poly.var_exists ((=) v) p then
        `DontKnow
      else
        `NoOp
    | AAssign (v, e, _) ->
      let is_nonneg = ai_is_nonneg ai_results.(node) in
      let pe = Poly.of_expr !e in
      if not (Poly.var_exists ((=) v) pe) then
        if Poly.var_exists ((=) v) p then
          `Reset (v, pe)
        else
          `NoOp
      else
      let p' = Poly.sub p (poly_subst v pe p) in
      let mp' = Poly.scale (-1.) p' in
      if Poly.is_const p' = Some 0. then
        `NoOp
      else if is_nonneg p' then
        `Decrement p'
      else if is_nonneg mp' then
        `Increment mp'
      else
        `DontKnow
    | _ -> `NoOp
  in

  let maxdecr p l =
    (* This sucks, we should be doing constants
       only.
    *)
    let module Jump = struct exception Out end in
    let jumpout _where = raise Jump.Out in
    let maxof pa pb =
      if Poly.compare pa pb = 0 then pa else
      if Poly.is_const pa = Some 0. then pb else
      if Poly.is_const pb = Some 0. then pa else
      match Poly.is_const (Poly.sub pa pb) with
      | Some k -> if k < 0. then pb else pa
      | None -> jumpout "maxof"
    in
    let hmemo = Hashtbl.create 11 in
    let rec go node maxd =
      try
        let maxd' = Hashtbl.find hmemo node in
        if Poly.compare maxd' maxd <> 0
        then jumpout "loop"
        else maxd
      with Not_found ->
        Hashtbl.add hmemo node maxd;
        let maxafter =
          List.fold_left (fun mx (act, dst) ->
            if dst = l.l_head
            || not (ISet.mem dst l.l_body)
            then mx
            else
              match classify node p act with
              | `Decrement d ->
                maxof mx (go dst (Poly.add d maxd))
              | `DontKnow ->
                jumpout "dontknow"
              | `NoOp | `Increment _ | `Reset _ ->
                maxof mx (go dst maxd)
          ) (Poly.zero ()) edges.(node)
        in
        Hashtbl.remove hmemo node;
        if Poly.is_const maxafter = Some 0.
        then maxd
        else maxafter
    in
    try go l.l_head (Poly.zero ())
    with Jump.Out -> Poly.const 1.
  in

  let cands_of_log loop log =
    List.fold_left (fun ps pbase ->
      let md = maxdecr pbase loop in
      if Poly.is_const md = Some 0. then ps else
      PSet.add (Poly.add md pbase) ps
    ) PSet.empty (PSet.elements (pset_of_logic log)) in

  iterloops begin fun l ->
    ISet.iter (fun node ->
      if List.exists
        (fun (_, dst) -> not (ISet.mem dst l.l_body))
        edges.(node)
      then begin
        (* One edge out of node exits the
           loop l *)
        let base ps =
          List.fold_left (fun ps (act, dst) ->
            if not (ISet.mem dst l.l_body) then ps else
            match act with
            | AGuard log ->
              PSet.union ps (cands_of_log l log)
            | _ -> ps
          ) ps edges.(node)
        in
        l.l_base <- base l.l_base;
      end
    ) l.l_body
  end;

  let lastl = ref None in

  let focus = ref [] in
  let add_focus f = focus := f :: !focus in

  let transl bs dst l =
    (* This takes a set of base functions relevant at 'dst'
       and attempts to express them at the loop 'l'.

       FIXME, this really only works with
       straight-line code.
    *)
    let rec go bs node =
      if node < l.l_head then PSet.empty else
      if ISet.mem node l.l_body then bs else
      List.fold_left (fun bs' pred ->
        if pred >= node then bs' else
        match
          List.find (fun (_, d) -> d = node) edges.(pred)
        with
        | AAssign (v, e, _), _ ->
          let bs =
            match desc !e with
            | ERandom ->
              PSet.filter
                (fun b -> not (Poly.var_exists ((=) v) b))
                bs
            | _ ->
              let pe = Poly.of_expr !e in
              pset_map (poly_subst v pe) bs
          in
          PSet.union bs' (go bs pred)
        | _ -> PSet.union bs' (go bs pred)
      ) bs preds.(node)
    in
    go bs dst
  in

  iterloops
  ~pre:begin fun l ->
    (* Translate all base functions needed
       after in the program source in the current
       loop's "language".  Then add them as bases.
    *)
    match !lastl with
    | None -> ()
    | Some ll ->
      l.l_base <- PSet.union l.l_base
        (transl ll.l_base ll.l_head l)
  end
  begin fun l ->

    let rec go stk bs =
      match stk with
      | [] -> bs
      | pcand :: stk
        when PSet.mem pcand bs || Poly.is_const pcand <> None
        -> go stk bs
      | pcand :: stk ->
        let stk = ref stk in
        add_focus (max0_ge_0 pcand);
        ISet.iter (fun node ->
          List.iter (fun (act, dst) ->
            if not (ISet.mem dst l.l_body) then () else
            match classify node pcand act with
            | `NoOp | `DontKnow -> ()
            | `Decrement pd ->
              let pdecr = Poly.sub pcand pd in
              add_focus (max0_ge_0 pdecr);
              add_focus (max0_monotonic (check_ge pcand pdecr));
              add_focus (max0_pre_decrement 1 pcand pd);
            | `Increment pi ->
              (* Look for conditions on the way to
                 the increment in the loop, use them
                 to infer new base functions.
                 TODO, some domination information
                 would be handy here.
              *)
              let rec upguard node =
                match preds.(node) with
                | [ pred ] ->
                  let (act, _) = List.find
                    (fun (_, d) -> d = node)
                    edges.(pred)
                  in
                  begin match act with
                  | AGuard log ->
                    stk :=
                      PSet.elements (cands_of_log l log)
                      @ !stk
                  | _ -> upguard pred
                  end
                | _ -> ()
              in
              upguard node;
              add_focus (max0_pre_increment 1 pcand pi);
            | `Reset (v, pe) ->
              (* Add a focus function to weaken our
                 candidate to the substitution result. *)
              let psubs = poly_subst v pe pcand in
              (* TODO, filter using AI results *)
              add_focus (max0_monotonic (check_ge pcand psubs));
              (* add [v, pe] and [pe, v] and subadditive
                 property for all bases depending on v *)
              stk := psubs :: !stk;
          ) edges.(node);
        ) l.l_body;
        go !stk (PSet.add pcand bs);
    in

    (* Aggregate all the bases of the child loops.  Those
       might be modified by the current current loop.
    *)
    let bases = List.fold_left
      (fun bs l -> PSet.union bs l.l_base)
      l.l_base l.l_chld
    in
    if l.l_head = -1 then
      (* Special case for the topmost "loop". *)
      match l.l_chld with
      | lfst :: _ ->
        PSet.iter
          (fun b -> add_focus (max0_ge_0 b))
          (transl bases lfst.l_head root)
      | _ -> ()
    else
      l.l_base <- go (PSet.elements bases) PSet.empty;
    lastl := Some l;

  end;

  let debug = false in

  if debug then begin (* Debug display of loop information. *)
    let open Format in
    let rec printl fmt l =
      if l.l_head = -1 then
        fprintf fmt "@[<v>Program root."
      else begin
        fprintf fmt "* @[<v>Loop %d." l.l_head;
        fprintf fmt "@ Norms: ";
        Print.list ~first:"@[<hov>" ~sep:",@ " ~last:"@]"
          Poly.print fmt (PSet.elements l.l_base);
        fprintf fmt "@ Body: ";
        Print.list ~first:"@[<hov>" ~sep:"@ " ~last:"@]"
          Format.pp_print_int fmt (ISet.elements l.l_body);
      end;
      if l.l_chld = [] then
        ()
      else begin
        fprintf fmt "@ Children loops:@   @[<v>";
        let fst = ref true in
        List.iter (fun l ->
          fprintf fmt
            (if !fst then "%a" else "@ %a")
            printl l;
          fst := false;
        ) l.l_chld;
        fprintf fmt "@]";
      end;
      fprintf fmt "@]";
    in
    eprintf "Function \x1b[1m%s\x1b[0m:@." gfunc.fun_name;
    eprintf "%a@.@." printl root
  end;

  let fun_focus =
    gfunc.fun_focus @ List.map export !focus |>
    List.filter (fun f -> Poly.is_const f.proves = None) |>
      List.sort (fun a b -> Poly.compare a.proves b.proves) |> dedup_sorted
  in

  if debug then begin
    Format.eprintf "Focus functions:@.  %a@.@."
      (Print.list ~first:"@[<v>" ~sep:"@ " ~last:"@]"
        Poly.print) (List.map (fun f -> f.proves) fun_focus);
  end;

  { gfunc with fun_focus }

(* Helper functions to create higher degree indices. *)

let rec prodfold n l ?(acc=[]) accf f =
  if n = 0 then f acc accf else
  match l with
  | [] -> accf
  | x :: l ->
    let rec iota i accf =
      if i > n then accf else
      let acc = (x, i) :: acc in
      iota (i+1) (prodfold (n-i) l ~acc accf f)
    in iota 0 accf

(* The heuristic to infer focus functions.
*)
let add_focus_old ?(degree=1) ai_results ai_get_nonneg _ gfunc =
  let pzero = Poly.zero () in

  let assigns =
    Array.fold_left
      (List.fold_left begin fun l (a, _) ->
          match a with
          | AAssign (_, e, _) when desc !e = ERandom -> l
          | AAssign (v, e, _) -> (v, Poly.of_expr !e) :: l
          | _ -> l
        end)
      [] gfunc.fun_body.g_edges
  in
  (* Collect all conditions used by the program. *)
  let base =
    Hashtbl.fold begin fun _ abs_array base ->
      Array.fold_left begin fun base abs ->
        List.fold_left
          (fun b p -> PSet.add p b) base
          (ai_get_nonneg abs)
      end base abs_array
    end ai_results PSet.empty
  in
  (* Close them under all the program assignments. *)
  let close acc (v, pe) =
    let mv = Monom.of_var v in
    if abs_float (Poly.get_coeff mv pe) = 1. then acc else
    PSet.fold (fun p -> PSet.add (poly_subst v pe p))
      base acc
  in
  let base = List.fold_left close base assigns in
  (* Remove constants. *)
  let base =
    PSet.filter (fun p -> Poly.is_const p = None) base in

  if false then
  PSet.iter (fun p ->
    Format.eprintf " %a@." Poly.print p) base;

  (* Create larger degree indices using product()
     and binom_monotonic().
  *)
  let rec binom n acc =
    if n = 0 then acc else
    let binom_max pol acc =
      binom_monotonic n
        (max0_ge_0 pol)
        (check_ge pzero pzero) ::
      binom_monotonic n
        (max0_ge_arg pol)
        (check_ge pol pzero) ::
      binom_monotonic n
        (max0_le_arg (check_ge pol pzero))
        (max0_ge_0 pol) ::
      acc
    in
    binom (n-1) (PSet.fold binom_max base acc)
  in
  let degn = binom degree [] in
  let base_list = PSet.elements base in
  let degn =
    let rec cross d degn acc =
      if false || d > degree then acc else
      cross (d + 1) degn begin
        List.fold_left (fun acc x ->
          prodfold (d - Focus_Builder.degree x) base_list acc
          begin fun prod acc ->
            List.fold_left
              (fun p (b, e) ->
                if e = 0 then p else
                product p
                  (binom_monotonic e
                    (max0_ge_0 b)
                    (check_ge pzero pzero))
              ) x prod :: acc
          end
        ) acc degn
      end
    in cross 2 degn degn
  in

  (* Add focus functions. *)
  let fun_focus = gfunc.fun_focus @ (List.map export degn) in
  (*
  List.iter (fun (l, p) ->
    if l = [] then
    Format.eprintf "%a@." Poly.print_ascii p) fun_focus;
  *)
  { gfunc with fun_focus }


(* Place weakening points automatically.
   A good heuristic is to insert them at
   the end of guard edges.
*)
let add_weaken ({ fun_name; fun_body = g } as gfunc) =
  let is_guard = function AGuard _ -> true | _ -> false in
  let weaken = ref [] in
  let nweaken = ref 0 in
  let add_weaken =
    let nnodes = Array.length g.g_edges in
    fun dst ->
      weaken := [AWeaken, dst] :: !weaken;
      incr nweaken;
      !nweaken - 1 + nnodes
  in
  let new_edges =
    Array.mapi begin fun src ->
      List.map begin fun (act, dst) ->
        let d_edges = g.g_edges.(dst) in
        if d_edges = [] || List.length d_edges > 1
        || is_guard act &&
           not (List.for_all (fun (a, _) -> is_guard a) d_edges)
        then (act, add_weaken dst)
        else (act, dst)
      end
    end g.g_edges
  in
  let weaken = Array.of_list (List.rev !weaken) in
  let new_position =
    Array.map begin function
      | [_, dst] -> g.g_position.(dst)
      | _ -> assert false
    end weaken
  in
  let fun_body =
    { g with
      g_edges = Array.append new_edges weaken;
      g_position = Array.append g.g_position new_position;
    } in
  stats.weaken_map <- (fun_name, !nweaken) :: stats.weaken_map;
  { gfunc with fun_body }
