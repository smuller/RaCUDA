(* Van Chan Ngo - 2018 *)

open Types
open Dist
open CS_Interop_Types

let fill_preds cfg_func_body = (* an array of blocks *)
  let preds = Array.make (Array.length cfg_func_body) [] in
  let addpred b_id i = 
    preds.(i) <- b_id :: preds.(i)
  in
  addpred (-1) 0; (* start block gets an extra predecessor with id -1 *)
  for n = 0 to Array.length cfg_func_body - 1 do
    match cfg_func_body.(n).bjump with
    | JRet _ | JRetVar _ -> ()
    | JJmp bs -> List.iter (addpred n) bs 
  done;
  Array.mapi (fun i b -> { b with bpreds = preds.(i) }) cfg_func_body

(* for testing *)
let rec id_lst_to_str l = 
  let rec aux l str no_comma = 
    match l with
    | [] -> str
    | h :: t -> 
      let comma = if no_comma then "" else "," in 
      aux t (str ^ comma ^ (Printf.sprintf "%s" h)) false
  in
  aux l "" true

let rec jmp_lst_to_str l = 
  let rec aux l str no_comma = 
    match l with
    | [] -> str
    | h :: t -> 
      let comma = if no_comma then "" else "," in 
      aux t (str ^ comma ^ (Printf.sprintf "b%d" h)) false
  in 
  aux l "" true

let inst_to_str ins = 
  match ins with
  | CSITick n ->
    Printf.sprintf "tick %d" n 
  | CSIAssert lg -> 
    (Printf.sprintf "assert ") ^ (Utils.logic_to_str lg)
  | CSIAssume lg -> 
    (Printf.sprintf "assume ") ^ (Utils.logic_to_str lg)
  | CSIAssign (id, e) ->
    (Printf.sprintf "%s = " id) ^ (Utils.expr_to_str e)
  | CSICall id -> 
    Printf.sprintf "%s()" id
  | CSICallArg (rt_l, id, id_l) -> 
    begin
      match rt_l with
      | [] -> 
        (Printf.sprintf "%s(" id) ^ (id_lst_to_str id_l) ^ ")"
      | _ -> 
        (id_lst_to_str rt_l) ^ (Printf.sprintf " = %s(" id) ^ (id_lst_to_str id_l) ^ ")"
    end
  | CSProbIf pe ->
    Utils.pexpr_to_str pe

let jump_to_str bjm = 
  match bjm with
  | JJmp id_l ->
    Printf.sprintf "goto " ^ (jmp_lst_to_str id_l)
  | JRetVar id_l ->
    begin 
      match id_l with
      | [] -> 
        Printf.sprintf "return []"
      | x :: [] ->
        Printf.sprintf "return %s" x
      | _ -> 
        Printf.sprintf "return (" ^ (id_lst_to_str id_l) ^ ")"
    end
  | JRet n ->
    Printf.sprintf "return"

let print_cfg_prg oc globals func_l =
  let print_cfg_inst ins = 
    Printf.fprintf oc "%s\n" (inst_to_str ins)
  in 

  let print_cfg_jump bjp = 
    Printf.fprintf oc "%s\n" (jump_to_str bjp)
  in 

  let print_cfg_blk blk_id blk = 
    Printf.fprintf oc "b%d:\n" blk_id;
    List.iter (print_cfg_inst) blk.binsts;
    print_cfg_jump blk.bjump
  in 

  let print_cfg_f fdes = 
    Printf.fprintf oc "func %s(" fdes.fname;
    let args_to_str = id_lst_to_str fdes.fargs in 
    Printf.fprintf oc "%s) " args_to_str;
    if (List.length (fdes.flocs)) > 0 then 
      begin
        Printf.fprintf oc "local(";
        let locs_to_str = id_lst_to_str fdes.flocs in 
        Printf.fprintf oc "%s) " locs_to_str;
      end;
    Printf.fprintf oc " (\n";
    Array.iteri (print_cfg_blk) fdes.fbody;
    Printf.fprintf oc ");\n\n";
  in  
  Printf.fprintf oc "global(";
  let globals_to_str = id_lst_to_str globals in 
  Printf.fprintf oc "%s)\n" globals_to_str;
  List.iter (print_cfg_f) func_l

(* get function by name *)
let get_f_by_name func_l fn = 
  try 
    List.find (fun x -> x.fname = fn) func_l
  with
    | Not_found -> 
      failwith (Printf.sprintf "invalid program: function %s not found" fn)

(* get all callee ids with arguments in fd *)
let get_callees fd = 
  let callee_l = ref [] in
  let do_f_ins ins = 
    match ins with
    | CSICallArg (_,id,_) -> 
      callee_l := id :: !callee_l
    | _ -> ()
  in

  let do_f_blk blk = 
    let n = List.length blk.binsts in
    for i = 0 to n - 1 do
      do_f_ins (List.nth blk.binsts i)
    done
  in

  let n = Array.length fd.fbody in
  for i = 0 to n - 1 do
    do_f_blk fd.fbody.(i)
  done;
  List.rev !callee_l

(* apply function f in a dfs order *)
let iter_dfs func_l fdes f = 
  let visited = Hashtbl.create 9 in 
  let rec dfs_util fd =  
    (* mark the current node is visited *)
    Hashtbl.add visited fd.fname true;
    (* do work here *)
    f fd;
    let callees = get_callees fd in 
    let n = List.length callees in 
    for i = 0 to n - 1 do
      let callee_i_n = List.nth callees i in 
      let callee_i = get_f_by_name func_l callee_i_n in
      try 
        (* already visited *)
        let _ = Hashtbl.find visited callee_i_n in ()
      with
      | Not_found ->
        (* visit all its callees *)
        dfs_util callee_i
    done
  in
  dfs_util fdes

(* check recursive call *)
let is_recursive func_l fdes = 
  let visited = Hashtbl.create 9 in
  let recstack = Hashtbl.create 9 in
  let rec is_recursive_util fdes = 
    if not (Hashtbl.mem visited fdes.fname) then
      begin
        Hashtbl.add visited fdes.fname true;
        Hashtbl.add recstack fdes.fname true;
        let callee_l = get_callees fdes in
        if (List.exists do_check callee_l) then 
          true
        else
          begin
            Hashtbl.remove recstack fdes.fname;
            false
          end
      end
    else
      begin
        Hashtbl.remove recstack fdes.fname;
        false   
      end  

  and do_check f_name = 
    let f_des = get_f_by_name func_l f_name in 
    if not (Hashtbl.mem visited f_name) && (is_recursive_util f_des) then
      true
    else if (Hashtbl.mem recstack f_name) then
      true
    else
      false
  in
  is_recursive_util fdes

(* clone a function if it is called more than 1 time.
 * the reason is that the callee potential signature is 
 * different each time it is called due to different context
 * this needs to be done before the transformation
 *)
let clone_multiple_call_util func_l count =
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
    Hashtbl.add h_func f.fname f
  in

  let add_clone_func fn f = 
    Hashtbl.add h_clone_func fn ({ f with fname = fn })
  in

  (* clone the callee *)
  let clone_callee rts fn args = 
    try
      let f = Hashtbl.find h_func fn in
      let clone_f_name = new_clone_n fn in
      add_clone_func clone_f_name f;
      CSICallArg (rts, clone_f_name, args)
    with
    (* the first time fn is called *)
    | Not_found -> 
      begin
        add_func (get_f_by_name func_l fn);
        CSICallArg (rts, fn, args)
      end
  in

  (* clone for each function *)
  let clone_f fdes = 
    let clone_f_ins ins = 
      match ins with
      | CSICallArg (rts,fn,args) -> 
        clone_callee rts fn args
      | _ -> 
        ins
    in

    let clone_f_blk blk = 
      let new_binsts = List.map (clone_f_ins) blk.binsts in
      { blk with binsts = new_binsts }
    in 

    let new_fbody = Array.map (clone_f_blk) fdes.fbody in
    { fdes with fbody = new_fbody }
  in

  let new_func_l = List.map (clone_f) func_l in
  (* no function is cloned *)
  if (Hashtbl.length h_clone_func) = 0 then 
    (new_func_l, !next_clone, false)
  else
    (Hashtbl.fold (fun k d init -> d :: init) h_clone_func new_func_l, !next_clone, true)

let clone_multiple_call func_l = 
  let rec aux func_l count again = 
    if again then
      let (new_func_l, new_count, new_again) = clone_multiple_call_util func_l count in
      aux new_func_l new_count new_again
    else
      func_l
  in
  aux func_l 0 true

(* transformation for function calls with arguments *)
let transform_callargs dump_blk globals func_l = 
  (* check for recursive call *)
  if (List.exists (is_recursive func_l) func_l) then
    failwith "invalid program: recursive call with arguments is not supported";

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

  let is_fassert_prob blk = 
    try 
      match (List.hd blk.binsts) with
      | CSIAssert _ | CSProbIf _ -> true
      | _ -> false
    with Failure _ -> false
  in

  (* transform callers *)
  let transform_caller fdes =     
    (* transform a call by adding assignments to registers and from return *)
    let transform_a_call rts fn args = 
      let ins_l = ref [] in 
      let create_assignmet_to_reg i arg = 
        let assign_reg = 
          if (Hashtbl.mem h_regs i) then 
            CSIAssign (Hashtbl.find h_regs i, EVar arg)
          else
            let reg = new_reg () in CSIAssign (reg, EVar arg)
        in
        ins_l := assign_reg :: !ins_l
      in
      List.iteri (create_assignmet_to_reg) args;
      
      (* add a call without arguments and return *)
      ins_l := (CSICall fn) :: !ins_l;

      (* add assignment for return *)
      let create_assignmet_from_ret i rt = 
        let assign_ret = 
          if (Hashtbl.mem h_rets i) then
            CSIAssign (rt, EVar (Hashtbl.find h_rets i))
          else
            let ret = new_ret () in CSIAssign (rt, EVar ret)
        in
        ins_l := assign_ret :: !ins_l
      in 
      List.iteri (create_assignmet_from_ret) rts;
      List.rev !ins_l
    in

    let transform_caller_ins ins = 
      match ins with
      | CSICallArg (rts,fn,args) -> 
        transform_a_call rts fn args
      | _ -> 
        [ins]
    in

    let rec transform_caller_il il =
      match il with
      | [] -> []
      | h::t -> 
        let new_t = transform_caller_il t in
        (transform_caller_ins h) @ new_t
    in

    let transform_caller_blk blk = 
      let new_binsts = transform_caller_il blk.binsts in
      { blk with binsts = new_binsts }
    in 

    let new_fbody = Array.map (transform_caller_blk) fdes.fbody in
    { fdes with fbody = new_fbody }
  in

  (* transform callee *)
  let transform_callee fdes = 
    (* add assignments from regisiters to argumetns *)
    let create_assignment_from_regs args = 
      if (List.length args = 0) then
       []
      else 
        begin
          let ins_l = ref [] in 
          let create_assignment_from_regs_util i arg = 
            let assign_reg = 
              if (Hashtbl.mem h_regs i) then 
                CSIAssign (arg, EVar (Hashtbl.find h_regs i))
              else
                let reg = new_reg () in CSIAssign (arg, EVar reg)
            in
            ins_l := assign_reg :: !ins_l
          in 
          List.iteri (create_assignment_from_regs_util) args;
          List.rev !ins_l
        end 
    in
    (* add assignment from return values to registers *)
    let create_assignment_to_rets rts = 
      let ins_l = ref [] in
      let create_assignment_to_rets_util i var_name = 
        let assign_ret = 
          if (Hashtbl.mem h_rets i) then 
            CSIAssign (Hashtbl.find h_rets i, EVar var_name)
          else
            let ret = new_ret () in CSIAssign (ret, EVar var_name)
        in
        ins_l := assign_ret :: !ins_l  
      in
      List.iteri (create_assignment_to_rets_util) rts;
      List.rev !ins_l
    in

    let create_assignment_for_bjump jump_ins =
      let new_return = JRet 0 in
      match jump_ins with
      | JRetVar rts ->
        if (List.length rts = 0) then
          failwith "invalid program: unvoid function must have return values"
        else
          let assign_rets = create_assignment_to_rets rts in 
          (Some assign_rets, new_return)
      | _ ->
        (None, jump_ins)
    in

    let h_blk_newbjump = Hashtbl.create 9 in
    let create_assignment_to_rets_blk blk_id blk = 
      let new_assignment_rets, new_bjump = create_assignment_for_bjump blk.bjump in
      match new_assignment_rets with
      | None -> blk
      | Some l ->
        Hashtbl.add h_blk_newbjump blk_id l;
        { blk with bjump = new_bjump }
    in 

    (* transform returns *)
    let new_fbody = Array.mapi (create_assignment_to_rets_blk) fdes.fbody in
    (* program is invalid if there are multiple exit points *)
    if (Hashtbl.length h_blk_newbjump > 1) then
      failwith "invalid program: multiple exit points";

    (* add new assignment from return to register *)
    if (Hashtbl.length h_blk_newbjump > 0) then
      begin 
        let append_to_end_of_block blk_id l = 
          let blk = new_fbody.(blk_id) in
          let new_binsts = blk.binsts @ l in 
          let new_blk = { blk with binsts = new_binsts } in
          new_fbody.(blk_id) <- new_blk
        in
        Hashtbl.iter (append_to_end_of_block) h_blk_newbjump
      end;

    (* add new asignments from registers to arguments at the begining of the first block *)
    let first_blk = new_fbody.(0) in 
    let new_assignment_regs = create_assignment_from_regs fdes.fargs in 
    let new_first_binsts = 
      if (is_fassert_prob first_blk) then 
        ((List.hd first_blk.binsts) :: new_assignment_regs) @ (List.tl first_blk.binsts)
      else
        new_assignment_regs @ first_blk.binsts
    in 

    let new_first_blk = { first_blk with binsts = new_first_binsts } in 
    new_fbody.(0) <- new_first_blk;

    (* remove all arguments and add them as local variables *)
    let new_locs = List.fold_left (fun init x -> x :: init) fdes.flocs fdes.fargs in 

    { fdes with flocs = new_locs; fargs = []; fbody = new_fbody }
  in  

  (* clone functions that are called multiple times first *)
  let new_func_l = clone_multiple_call func_l in

  (* then, transform the callers *)
  let new_func_l = List.map transform_caller new_func_l in

  (* next, transform the callees *)
  let new_func_l = List.map transform_callee new_func_l in 

  (* finally, make all registers and returns to be global variables *)
  let regs = Hashtbl.fold (fun k d init -> d :: init) h_regs [] in
  let rets = Hashtbl.fold (fun k d init -> d :: init) h_rets [] in 
  let new_globals = (globals @ regs) @ rets in 

  (* for testing *)
  if dump_blk then 
  begin
    let oc = open_out "blk_prog.cfg" in 
    print_cfg_prg oc new_globals new_func_l;
    close_out oc
  end;

  new_globals, new_func_l
