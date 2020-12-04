(* Quentin Carbonneaux - 2016-2017 *)

open Types
include IMP_Types

let parse_file f =
  let lexbuf = Lexing.from_channel (open_in f) in
  IMP_Lexer.init lexbuf f;
  try
    let g, l = IMP_Grammar.file IMP_Lexer.lex lexbuf in
    g, List.map (fun f ->
      { f with fun_focus = List.map Focus.interpret f.fun_focus }) l
  with Parsing.Parse_error ->
    let startp = Lexing.lexeme_start_p lexbuf in
    Format.eprintf "%s:%i:%i: syntax error near '%s'@."
      startp.Lexing.pos_fname
      startp.Lexing.pos_lnum
      (startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1)
      (Lexing.lexeme lexbuf);
    raise Utils.Error

(* get function by name *)
let get_f_by_name func_l fn =
  try
    Some (List.find (fun x -> x.fun_name = fn) func_l)
  with Not_found -> None

(* get all callees with arguments *)
let get_callees fd = 
  let rec goi ins = 
      match ins with 
      | IIf (log, bi, be) ->
        let be_l = gob be in 
        (gob bi) @ be_l
       
      | IWhile (log, b) ->
        (gob b)
      
      | ILoop b ->
        (gob b)

      | IProbIf (prob_e, bl, br) -> 
        let br_l = gob br in
        (gob bl) @ br_l

      | ICallArg (rl, fn, argl) -> 
        [fn]

      | _ -> [] 
        
    and goil il = 
      match il with 
      | [] -> []
      | (i, pos) :: b ->
        let b_l = goil b in
        (goi i) @ b_l

    and gob blk =
      let body = blk.b_body in 
      goil body
    in

    gob fd.fun_body

(* check recursive call *)
let is_recursive func_l fd = 
  let visited = Hashtbl.create 9 in
  let recstack = Hashtbl.create 9 in
  let rec is_recursive_util fdes = 
    if not (Hashtbl.mem visited fdes.fun_name) then 
      begin
        Hashtbl.add visited fdes.fun_name true;
        Hashtbl.add recstack fdes.fun_name true;
        let callee_l = get_callees fdes in
        let check = List.exists do_check callee_l in
        if (check) then
          true
        else 
          begin
            Hashtbl.remove recstack fdes.fun_name;
            false
          end
      end
    else
      begin 
        Hashtbl.remove recstack fdes.fun_name;
        false
      end

  and do_check f_name =
    match get_f_by_name func_l f_name with
    | None -> false
    | Some f_des ->
    if not (Hashtbl.mem visited f_name) && (is_recursive_util f_des) then
      true
    else if (Hashtbl.mem recstack f_name) then 
      true
    else
      false
  in
  is_recursive_util fd

(* clone a function if it is called more than 1 time.
 * this needs to be done before the transformation
 *)
let multiple_call_clone func_l count =
  let next_clone = ref count in
  let clone_name = "_cle_" in
  let new_clone_n fn = 
    let new_clone_name = fn ^ clone_name ^ (string_of_int !next_clone) in
    incr next_clone;
    new_clone_name
  in

  let h_func = Hashtbl.create 9 in
  let h_clone_func = Hashtbl.create 9 in 
  
  let add_func f = 
    Hashtbl.add h_func f.fun_name f
  in
  
  let add_clone_func fn f = 
    Hashtbl.add h_clone_func fn ({ f with fun_name = fn})
  in 

  (* get a function by name *)
  let get_func_opt fn = 
    try
      Some (List.find (fun x -> x.fun_name = fn) func_l)
    with Not_found -> None
  in

  let f_transformation impf = 
    (* clone the callee *)
    let clone_callee pos rl fn argl = 
      try 
        let f = Hashtbl.find h_func fn in
        let clone_f_name = new_clone_n fn in
        (* clone f *)
        add_clone_func clone_f_name f; 
        [(ICallArg (rl, clone_f_name, argl), pos)]
      with
      (* the first time fn is called *) 
      | Not_found ->
         begin
           match (get_func_opt fn, rl) with
           | (Some fn_des, _) ->
              (add_func fn_des;
               [(ICallArg (rl, fn, argl), pos)])
           (* treat as an uninterpreted function *)
           | (None, [ret]) -> [(ICallUninterp (ret, fn, argl), pos)]
           | (None, _) -> failwith "invalid program: uninterpreted functions can have only one return value"
        end
    in

    (* transform for one instruction *)
    let rec goi pos ins = 
      match ins with 
      | IIf (log, bi, be) ->
        let new_bi = gob bi in 
        let new_be = gob be in 
        [(IIf (log, new_bi, new_be), pos)] 
       
      | IWhile (log, b) ->
        let new_b = gob b in 
        [(IWhile (log, new_b), pos)]
      
      | ILoop b ->
        let new_b = gob b in 
        [(ILoop new_b, pos)]

      | IProbIf (prob_e, bl, br) -> 
        let new_bl = gob bl in 
        let new_br = gob br in 
        [(IProbIf (prob_e, new_bl, new_br), pos)]

      | ICallArg (rl, fn, argl) -> 
        (* recursive function *)
        if (fn = impf.fun_name) then
          [ICallArg (rl, fn, argl),pos]
        else
          clone_callee pos rl fn argl

      | _ -> [(ins, pos)] 
        
    and goil il = 
      match il with 
      | [] -> []
      | (i, pos) :: b ->
        let new_b = goil b in
        (goi pos i) @ new_b

    and gob blk =
      let body = blk.b_body in 
      { blk with b_body = (goil body) }
    in

    let new_body = gob impf.fun_body in 
    { impf with fun_body = new_body }
  in 

  let new_func_l = List.map f_transformation func_l in 
  if ((Hashtbl.length h_clone_func) = 0) then
    (new_func_l, !next_clone, false)
  else 
    (Hashtbl.fold (fun k d init -> d :: init) h_clone_func new_func_l, !next_clone, true)

let multiple_call_transformation func_l =
  let rec aux func_l count again = 
    if again then 
      let (new_func_l, new_count, new_again) = multiple_call_clone func_l count in 
      aux new_func_l new_count new_again
    else
      func_l
  in 
  aux func_l 0 true


(* transform function calls with arguments and return values 
 * to use global registers 
 * globals : list of global variables
 * func_l : list of function
 * we do the transformation based on depth-first-search (DFS) of the call graph
 *)
let function_call_transformation globals func_l = 


  let h_regs = Hashtbl.create 9 in
  let h_rets = Hashtbl.create 9 in

  let next_reg = ref 0 in
  let reg_name = "_reg_" in
  let new_reg () = 
    let reg = reg_name ^ (string_of_int !next_reg) in
    Hashtbl.add h_regs !next_reg reg;
    incr next_reg;
    reg
  in

  let next_ret = ref 0 in
  let ret_name = "_ret_" in
  let new_ret () =
    let ret = ret_name ^ (string_of_int !next_ret) in
    Hashtbl.add h_rets !next_ret ret;
    incr next_ret;
    ret
  in

  (*
  let diff l1 l2 = 
    List.filter (fun x -> not (List.mem x l2)) l1 
  in
  *)

  (* transformation of function calls *)
  let args_transformation impf = 
    let regs_rets_for_args pos args fn rets =
      let ins_l = ref [] in 
      for i = 0 to ((List.length args) - 1) do
        try 
          let reg = Hashtbl.find h_regs i in
          let _ = CUDA.add_var_exp reg (EVar (List.nth args i)) in
          let assign_reg = (IAssign (reg, EVar (List.nth args i)), pos) in 
          ins_l := assign_reg :: !ins_l
        with 
        | Not_found ->
           let reg = new_reg () in
           let _ = CUDA.add_var_exp reg (EVar (List.nth args i)) in
          let assign_reg = (IAssign (reg, EVar (List.nth args i)), pos) in 
          ins_l := assign_reg :: !ins_l
      done;
      
      (* add a call without arguments and returns *)
      ins_l := ((ICall fn), pos) :: !ins_l;
      
      for j = 0 to ((List.length rets) - 1) do 
        try
          let ret = Hashtbl.find h_rets j in 
          let assign_ret = (IAssign ((List.nth rets j), (EVar ret)), pos) in 
          ins_l := assign_ret :: !ins_l
        with
        | Not_found -> 
          let ret = new_ret () in
          let assign_ret = (IAssign ((List.nth rets j), (EVar ret)), pos) in 
          ins_l := assign_ret :: !ins_l
      done;
      List.rev !ins_l
    in

    (* transform for one instruction *)
    let rec goi pos ins = 
      match ins with 
      | IIf (log, bi, be) ->
        let new_bi = gob bi in 
        let new_be = gob be in 
        [(IIf (log, new_bi, new_be), pos)] 
       
      | IWhile (log, b) ->
        let new_b = gob b in 
        [(IWhile (log, new_b), pos)]
      
      | ILoop b ->
        let new_b = gob b in 
        [(ILoop new_b, pos)]

      | IProbIf (prob_e, bl, br) -> 
        let new_bl = gob bl in 
        let new_br = gob br in 
        [(IProbIf (prob_e, new_bl, new_br), pos)]

      | ICallArg (rl, fn, argl) -> 
        regs_rets_for_args pos argl fn rl

      | _ -> [(ins, pos)] 
        
    and goil il = 
      match il with 
      | [] -> []
      | (i, pos) :: b ->
        let new_b = goil b in
        (goi pos i) @ new_b

    and gob blk =
      let body = blk.b_body in 
      { blk with b_body = (goil body) }
    in

    let new_body = gob impf.fun_body in 
    { impf with fun_body = new_body }
  in 

  (* transformation of returns *)
  let rets_transformation impf = 
    let creat_args pos args = 
      let ins_l = ref [] in 
      for i = 0 to ((List.length args) - 1) do
        try 
          let reg = Hashtbl.find h_regs i in
          let _ = CUDA.add_var_exp reg (EVar (List.nth args i)) in
          let assign_arg = (IAssign ((List.nth args i), EVar reg), pos) in 
          ins_l := assign_arg :: !ins_l
        with 
        | Not_found ->
           let reg = new_reg () in
           let _ = CUDA.add_var_exp reg (EVar (List.nth args i)) in
          let assign_arg = (IAssign ((List.nth args i), EVar reg), pos) in 
          ins_l := assign_arg :: !ins_l
      done;
      List.rev !ins_l
    in 

    let creat_rets pos ret_l = 
      let ins_l = ref [] in 
      for i = 0 to ((List.length ret_l) - 1) do
        try 
          let ret = Hashtbl.find h_rets i in
          let assign_ret = (IAssign (ret, EVar (List.nth ret_l i)), pos) in 
          ins_l := assign_ret :: !ins_l
        with 
        | Not_found ->
          let ret = new_reg () in
          let assign_ret = (IAssign (ret, EVar (List.nth ret_l i)), pos) in 
          ins_l := assign_ret :: !ins_l
      done;
      List.rev !ins_l
    in 

    (* transform for one instruction *)
    let rec goi pos ins = 
      match ins with 
      | IIf (log, bi, be) ->
        let new_bi = gob bi in 
        let new_be = gob be in 
        [(IIf (log, new_bi, new_be), pos)] 
       
      | IWhile (log, b) ->
        let new_b = gob b in 
        [(IWhile (log, new_b), pos)]
      
      | ILoop b ->
        let new_b = gob b in 
        [(ILoop new_b, pos)]

      | IProbIf (prob_e, bl, br) -> 
        let new_bl = gob bl in 
        let new_br = gob br in 
        [(IProbIf (prob_e, new_bl, new_br), pos)]

      | IReturn ret_l -> 
        creat_rets pos ret_l

      | _ -> [(ins, pos)] 
        
    and goil il = 
      match il with 
      | [] -> []
      | (i, pos) :: b ->
        let new_b = goil b in
        (goi pos i) @ new_b

    and gob blk =
      let body = blk.b_body in 
      { blk with b_body = (goil body) }
    in

    (* add assignments from registers to arguments *)
    let new_body = gob impf.fun_body in 
    let new_body_ins = (creat_args impf.fun_start_p impf.fun_args) @ new_body.b_body in  

    (* remove all arguments and add them as local variables *)
    let new_fun_vars = List.fold_left (fun init x -> x :: init) impf.fun_vars impf.fun_args in 

    { impf with fun_vars = new_fun_vars; fun_body = { new_body with b_body = new_body_ins; }; fun_args = [] }
  in 

  (* check recursive *)
  if (List.exists (is_recursive func_l) func_l) then
    failwith "invalid program: recursive call with arguments is not supported";

  (* make local variables that are not passed as arguments to be globals *)
  
  (* transform multiple calls first *)
  let new_func_l = multiple_call_transformation func_l in 
  
  (* then transform function call *)
  let new_func_l = List.map args_transformation new_func_l in 
  
  (* finally, transform returns *)
  let new_func_l = List.map rets_transformation new_func_l in 
  
  (* make all registers and returns to be global variables *)
  let regs = Hashtbl.fold (fun k d init -> d :: init) h_regs [] in 
  let rets = Hashtbl.fold (fun k d init -> d :: init) h_rets [] in 
  let new_globals = (globals @ regs) @ rets in 
  
  new_globals, new_func_l
  
let written_instr (i: instr) : id list =
  match i with
  | IAssign (id, _) -> [id]
  | _ -> []

let written_block (b: block) : id list =
  List.flatten (List.map (fun (i, _) -> written_instr i) b.b_body)
