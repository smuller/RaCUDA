(* Vilhelm Sjoberg & Quentin Carbonneaux - 2017 *)
(* Van Chan Ngo - 2017 *)

open Types
open Focus_Types
open Graph_Types
open Presburger
open Polynom

let globs = ref []

let coq_escape s =
  let rec escape r j =
    if j = -1 then String.concat "" r else
    match s.[j] with
    | '.' -> escape ("_dot_" :: r) (j-1)
    | c -> escape (String.make 1 c :: r) (j-1)
  in
  if String.contains s '.'
  then escape [] (String.length s - 1)
  else s

let procname x = "P_" ^ x
let mkvarname funname x =
  if List.mem x !globs
  then "G_" ^ coq_escape x
  else "V_" ^ funname ^ "_" ^ coq_escape x
let statevar s varname x =
  s ^ " " ^ varname x

(* Assumes that we have a state s in the context. *)
let rec dump_expr_norand varname fmt e =
  match desc e with
  | ERandom ->
    failwith "ERandom in invalid context"
  | EVar x ->
    Format.fprintf fmt "(EVar %s)" (varname x)
  | ENum n ->
    Format.fprintf fmt "(ENum (%d))" n
  | EAdd (e1, e2) ->
     Format.fprintf fmt "(EAdd %a@ %a)"
       (dump_expr_norand varname) e1
       (dump_expr_norand varname) e2
  | ESub (e1, e2) ->
    Format.fprintf fmt "(ESub %a@ %a)"
      (dump_expr_norand varname) e1
      (dump_expr_norand varname) e2
  | EMul (e1, e2) ->
    Format.fprintf fmt "(EMul %a@ %a)"
      (dump_expr_norand varname) e1
      (dump_expr_norand varname) e2
  | EBer (_, _) | EBin (_, _, _) | EGeo (_, _) 
  | ENbin (_, _, _) | EPois (_, _) | EHyper (_, _, _) 
  | EUnif (_, _) | EDist (_, _, _, _, _) -> 
    Utils._TODO "Coq proof for probabilistic program is not supported."

let dump_expr varname fmt e =
  match desc e with
  | ERandom ->
    Format.fprintf fmt "None"
  | _ ->
    Format.fprintf fmt "(Some %a)"
      (dump_expr_norand varname) e

(* Assumes that we have a state s in the context. *)
let rec dump_logic varname fmt = function
  | LRandom | LTrue ->
    Format.fprintf fmt "True"
  | LFalse ->
    Format.fprintf fmt "False"
  | LCmp (e1,cmp,e2) ->
    let cmp = match cmp with
      | Le -> "<="
      | Lt -> "<"
      | Ge -> ">="
      | Gt -> ">"
      | Eq -> "="
      | Ne -> "<>"
    in
    Format.fprintf fmt "((eval %a@ s) %s@ (eval %a@ s))%%Z"
      (dump_expr_norand varname) e1
      cmp
      (dump_expr_norand varname) e2
  | LAnd (l1,l2) ->
    Format.fprintf fmt "(%a@ /\\ %a)"
      (dump_logic varname) l1
      (dump_logic varname) l2
  | LOr (l1,l2) ->
    Format.fprintf fmt "(%a@ \\/ %a)"
      (dump_logic varname) l1
      (dump_logic varname) l2
  | LNot l ->
     Format.fprintf fmt "(~ %a)"
       (dump_logic varname) l

let dump_edges varname fmt es =
  let dump_edge s s' fmt =
    function
    | ANone ->
      Format.fprintf fmt "EA %d ANone %d" s s'
    | AWeaken ->
      Format.fprintf fmt "EA %d AWeaken %d" s s'
    | AGuard a ->
      Format.fprintf fmt "EA %d (AGuard@ (fun s => %a)) %d"
        s (dump_logic varname) a s'
    | AAssign (x, e, _) ->
      Format.fprintf fmt "EA %d (AAssign@ %s@ %a) %d"
        s (varname x) (dump_expr varname) !e s'
    | ACall f ->
      Format.fprintf fmt "EC %d %s %d"
        s (procname f) s'
    | AProb p -> 
      Utils._TODO "Coq proof for probabilistic program is not supported."
  in
  Format.fprintf fmt "@[<hov>";
  Array.iteri (fun s ->
    List.iter (fun (a,s') ->
      Format.fprintf fmt "(%a)::@," (dump_edge (s+1) (s'+1)) a
    )
  ) es;
  Format.fprintf fmt "nil";
  Format.fprintf fmt "@]"

let dump_program fmt (_, fs) =
  let nglos = List.length !globs in

  (* Procedures ADT and Global variables *)
  Format.fprintf fmt
    "Inductive proc: Type :=@,  %a.@,@,"
    (Print.list ~first:"@[<hov>" ~sep:"@ |@ " ~last:"@]"
      (fun fmt f -> Format.fprintf fmt "%s" (procname f.fun_name))
    ) fs;
  List.iteri (fun i x ->
    Format.fprintf fmt "Notation G_%s := %d%%positive.@,"
      x (1 + i)
  ) !globs;
  Format.fprintf fmt
    "Definition var_global (v: id): bool :=@,  \
       match v with@,  @[<v>%a| _ => false@]@,  end.@,"
    (fun fmt ->
      List.iter (fun v ->
        Format.fprintf fmt "| G_%s => true@," v))
    !globs;

  (* Edges, and entry and exit point of procedures *)
  let dump_proc f =
    let varname = (mkvarname f.fun_name) in
    Format.fprintf fmt "@,@[<v>";
    List.iteri (fun i x ->
      Format.fprintf fmt "Notation %s := %d%%positive.@,"
        (varname x) (1 + i + nglos)
    ) f.fun_vars;
    Format.fprintf fmt "Definition Pedges_%s: list (edge proc) :=@,  %a.@,"
      f.fun_name (dump_edges varname) f.fun_body.g_edges;
    Format.fprintf fmt "@]";
  in
  List.iter dump_proc fs;

  (* Finally, the program Instance *)
  let dump_match fmt m =
    Format.fprintf fmt "@[<v>match p with@,%aend@]"
      (fun fmt ->
        List.iter (fun ({fun_name=fn; _} as f) ->
          assert (f.fun_body.g_start = 0);
          Format.fprintf fmt "| %s => %s@,"
            (procname fn) (m f)))
      fs
  in
  Format.fprintf fmt "@,Instance PROG: Program proc := {@,";
  Format.fprintf fmt "  proc_edges := fun p =>@,    %a;@,"
    dump_match (fun f -> "Pedges_" ^ f.fun_name);
  Format.fprintf fmt "  proc_start := fun p => 1%%positive;@,";
  Format.fprintf fmt "  proc_end := fun p =>@,    (%a)%%positive;@,"
    dump_match (fun f -> string_of_int (1+f.fun_body.g_end));
  Format.fprintf fmt "  var_global := var_global@,";
  Format.fprintf fmt "}.@,";
  ()

let dump_ai print_bound fmt fn ai_annots =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@,Definition ai_%s (p: node) (s: state): Prop := @," fn;
  Format.fprintf fmt "  (@[<v>match p with@,";
  let varname = statevar "s" (mkvarname fn) in
  let print_bound = print_bound varname in
  Array.iteri (fun i v ->
    Format.fprintf fmt "| %d => (%a)%%Z@,"
      (i+1) print_bound v
  ) ai_annots;
  Format.fprintf fmt "| _ => False@,end@])%%positive.@,"

let eps = 1e-4

let int_of_flt x =
  let c = ceil x and f = floor x in
  let dc = c -. x and df = x -. f in
  let check_ratios d1 d2 y =
    if (abs_float d1 >= eps || abs_float d2 >= eps)
    && d1 /. eps >= d2 then
      Printf.eprintf "Coq Generation: Warning trying \
                      to convert %g to an integer!\n" x;
    int_of_float y;
  in
  if dc < df
  then (check_ratios dc df c)
  else (check_ratios df dc f)

let rat_of_flt x =
  let sign, x = if x < 0. then -1, -. x else +1, +. x in
  let intp = floor x in
  let x = x -. intp in
  let farey n x =
    let rec go (a, b) (c, d) =
      if b > n then
        (c, d)
      else if d > n then
        (a, b)
      else
        let mediant =
          float_of_int (a+c) /.
          float_of_int (b+d) in
        if abs_float (x -. mediant) < eps then
          if b + d <= n then
            (a+c, b+d)
          else if d > b then
            (c, d)
          else
            (a, b)
        else if x > mediant then
          go (a+c, b+d) (c, d)
        else
          go (a, b) (a+c, b+d)
    in
    go (0, 1) (1, 1)
  in
  let (n, d) = farey 200 x in
  (sign * (n + int_of_float intp * d), d)

let rec dump_poly ring varname fmt pol =
  let print_coeff =
    match ring with
    | `Q -> begin fun fmt c ->
        let n, d = rat_of_flt c in
        Format.fprintf fmt "(%d # %d)" n d
      end
    | `Z -> begin fun fmt c ->
        Format.fprintf fmt "%d" (int_of_flt c)
      end
  in
  Format.fprintf fmt "@[<hov>";
  let is_zero = Poly.fold begin fun monom k first ->
      let pref, k =
        if k < 0.
        then "-", (-. k)
        else (if first then "" else "+"), k
      in
      if Monom.is_one monom then
        Format.fprintf fmt
          (if first then "%s%a" else "@ %s %a")
          pref print_coeff k
      else if abs_float (k -. 1.) < eps then
        Format.fprintf fmt
          (if first then "%s%a" else "@ %s %a")
          pref (dump_monom ring varname) monom
      else
        Format.fprintf fmt
          (if first then "%s@[<h>%a * %a@]" else "@ %s @[<h>%a * %a@]")
          pref print_coeff k (dump_monom ring varname) monom;
        false
    end pol true in
  if is_zero then Format.fprintf fmt "0";
  Format.fprintf fmt "@]"

and dump_monom ring varname fmt m =
  Format.fprintf fmt "@[<h>";
  let is_one = Monom.fold begin fun f e first ->
      if e = 0 then first else begin
      if e = 1 then
        Format.fprintf fmt
          (if first then "%a" else "@ *@ %a")
          (dump_factor ring varname) f
      else
        Format.fprintf fmt
          (if first then "%a^%d" else "@ *@ %a^%d")
          (dump_factor ring varname) f e;
        false
      end
    end m true in
  if is_one then Format.fprintf fmt "1";
  Format.fprintf fmt "@]"

and dump_factor ring varname fmt = function
  | Factor.Var v ->
    Format.fprintf fmt "%s" (varname v)
  | Factor.Max p ->
    Format.fprintf fmt "max0(%a)"
      (dump_poly `Z varname) p

let dump_hint varname fmt ((ka, kb), f) =
  let rec dump_ast fmt =
    let print s = Format.fprintf fmt s in
    let poly = dump_poly `Z varname in
    function
    | F_one -> print "F_one"
    | F_check_ge (a, b) ->
      print "F_check_ge (%a) (%a)" poly a poly b
    | F_max0_pre_decrement (k, a, b) ->
      print "F_max0_pre_decrement %d (%a) (%a)" k poly a poly b
    | F_max0_pre_increment (k, a, b) ->
      print "F_max0_pre_increment %d (%a) (%a)" k poly a poly b
    | F_max0_ge_0 a ->
      print "F_max0_ge_0 (%a)" poly a
    | F_max0_ge_arg a ->
      print "F_max0_ge_arg (%a)" poly a
    | F_max0_le_arg f ->
      print "F_max0_le_arg (%a)" dump_ast f
    | F_max0_monotonic f ->
      print "F_max0_monotonic (%a)" dump_ast f
    | F_max0_sublinear f ->
      print "F_max0_sublinear (%a)" dump_ast f
    | F_binom_monotonic (k, f, g) ->
      print "F_binom_monotonic %d (%a) (%a)" k dump_ast f dump_ast g
    | F_product (f, g) ->
      print "F_product (%a) (%a)" dump_ast f dump_ast g
  in
  Format.fprintf fmt "@[<h>(*%g %g*) %a@]" ka kb dump_ast f.ast

let dump_annots fmt fn annots =
  let varname = statevar "s" (mkvarname fn) in
  let dump_annot i annot_hints =
    Format.fprintf fmt
      "@[<v>@,Definition annot%d_%s \
              (p: node) (z: Q) (s: state): Prop := @," i fn;
    Format.fprintf fmt "  (@[<v>match p with@,";
    Array.iteri (fun i (v, hl) ->
      let used ((ka, kb), _) = abs_float (ka -. kb) > fsmall in
      let hl = List.filter used hl in
      if hl = [] then
        Format.fprintf fmt "| %d => (%a <= z)%%Q@,"
          (i+1) (dump_poly `Q varname) v
      else
        Format.fprintf fmt "| %d => hints@,  %a@,  (%a <= z)%%Q@,"
          (i+1) (Print.list (dump_hint varname)) hl
          (dump_poly `Q varname) v
    ) annot_hints;
    Format.fprintf fmt "| _ => False@,end)%%positive.@]@,";
    Format.fprintf fmt "@]"
  in
  List.iteri dump_annot annots;
  ()

let dump_ipa fmt annots flist =
  Format.fprintf fmt "@,Definition ipa: IPA := fun p =>@,  ";
  Format.fprintf fmt "@[<v>match p with@,";
  List.iter (fun {fun_name = fn; _} ->
    let l = try Hashtbl.find annots fn with Not_found -> [] in
    if l = [] then
      Format.fprintf fmt "| %s => []@," (procname fn) else
    let n = List.length l in
    Format.fprintf fmt "| %s =>@,  [@[<v>" (procname fn);
    for i = 0 to n - 1 do
      Format.fprintf fmt
        "mkPA Q (fun n z s => ai_%s n s /\\ annot%d_%s n z s)"
        fn i fn;
      if i = n - 1 then
        Format.fprintf fmt "@]]@,"
      else
        Format.fprintf fmt ";@,"
    done;
  ) flist;
  Format.fprintf fmt "end.@]@,@,";
  Format.fprintf fmt
    "Theorem admissible_ipa: IPA_VC ipa.@,\
     Proof.@,  prove_ipa_vc.@,Qed.@,";
  ()

let dump generated_proof_name fstart prog qry bnd print_bound ai_results annots =
  globs := (fst prog);
  let oc = open_out generated_proof_name in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "@[<v>Require Import pastis.Pastis.@,@,";
  dump_program fmt prog;
  Hashtbl.iter (dump_ai print_bound fmt) ai_results;
  Hashtbl.iter (dump_annots fmt) annots;
  dump_ipa fmt annots (snd prog);
  Format.fprintf fmt
    "@,Theorem bound_valid:@,  \
       forall s1 s2, steps %s (proc_start %s) s1 (proc_end %s) s2 ->@,    \
         (%a <= %a)%%Q.@,\
     Proof.@,  prove_bound ipa admissible_ipa %s.@,Qed."
    (procname fstart) (procname fstart) (procname fstart)
    (dump_poly `Q (statevar "s2" (mkvarname fstart))) qry
    (dump_poly `Q (statevar "s1" (mkvarname fstart))) bnd
    (procname fstart);
  Format.fprintf fmt "@.@]";
  close_out oc
