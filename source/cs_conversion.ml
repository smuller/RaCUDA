open InterIR
open Types
open CS_Interop_Types
open Printf
open String


type blkdesc = csblock
type funcdesc = csblock array csfunc

exception Unexpected_value of string

let glob_map = ref []
let current_loc_map = ref []
let current_arg_map = ref []
let procs_lst = ref []
let main_func = ref {fname = " main "; flocs = []; fargs = []; fbody = (Array.of_list [])}
let cfg_out_file = "intercfg.txt"
let fun_param = "fun_param_"

type file = {
  mutable filename : string;
  mutable funcs : funcdesc list;
  mutable glos : Types.id list;
  mutable main : funcdesc;
}

let counter= ref 0
let get_fresh_variable ()= counter:=!counter+1; fun_param ^ (string_of_int !counter)

let convert_variable (name,typ) =name

let get_lvalue_var lval =
  match lval with
    Var(va) -> convert_variable va
  | _ ->raise (Unexpected_value "found something other than a variable on the left side of an assigment")

let convert_bexpr op l r =
  mk ()
    (match op with 
       Add -> Types.EAdd(l,r)
     | Sub -> Types.ESub(l,r)
     | Mult -> Types.EMul(l,r)
     | _ -> Types.ERandom)

let convert_cop op =
  match op with
    GTE -> Types.Ge
  | GT -> Types.Gt
  | LTE -> Types.Le
  | LT -> Types.Lt
  | NE -> Types.Ne
  | EQ -> Types.Eq
  | _ -> raise (Unexpected_value "Tried to transform non-relational operator into relational ")

let convert_uexpr op r=
  match op with
    UNeg -> mk () (Types.ESub( mk () (Types.ENum(0)),r))
  | LNeg -> mk () Types.ERandom

let convert_lvalue l : unit Types.expr=
  match l with
    Var(variable) -> mk () (EVar(convert_variable variable))
  | Undef -> mk () (Types.ERandom)
  | FieldAccess(lvalue,field) -> raise (Unexpected_value ("Unexpected field access: "^field))

let rec convert_rexpr expr =
  match expr with
  | Constant(value,_) -> mk () (Types.ENum(value))
  | LVal(lval) -> convert_lvalue lval
  | BExpr(lhs,op,rhs)-> if(is_relational_op op) || (is_logical_op op) then
                          mk () (Types.ERandom)
                        else
                          convert_bexpr op (convert_rexpr lhs) (convert_rexpr rhs)
  | UExpr(op,r) -> convert_uexpr op (convert_rexpr r)
  | Multiple(_) -> raise (Unexpected_value "Unexpected multiple rexpr ")

let rec convert_logical_rexpr expr=
  match expr with
  | BExpr(lhs,op,rhs)->( match op with
                          GTE| GT| LTE |LT |NE |EQ ->
                                LCmp(convert_rexpr lhs,convert_cop op,convert_rexpr rhs)
                          | And ->
                             LAnd(convert_logical_rexpr lhs,convert_logical_rexpr rhs)
                          | Or ->
                             LOr(convert_logical_rexpr lhs,convert_logical_rexpr rhs)
                          | _ ->raise (Unexpected_value "Found non-boolean expression in condition")
                       )
  | LVal(Undef) -> Types.LRandom
  | UExpr(LNeg,rhs) -> LNot (convert_logical_rexpr rhs)
  | _ -> raise (Unexpected_value "Found non-boolean expression in condition")

let switch_op cop =
  match cop with
    Types.Ge -> Types.Lt
  | Types.Gt -> Types.Le
  | Types.Le -> Types.Gt
  | Types.Lt -> Types.Ge
  | Types.Ne -> Types.Eq
  | Types.Eq -> Types.Ne

let rec negate_logical logic=
  match logic with
  | LTrue -> LFalse
  | LFalse -> LTrue
  | LRandom -> LRandom
  | LCmp(lhs,op,rhs) ->LCmp(lhs,switch_op op,rhs)
  | LAnd(lhs,rhs) ->LOr(negate_logical lhs,negate_logical rhs)
  | LOr(lhs,rhs) ->LAnd(negate_logical lhs,negate_logical rhs)
  | LNot(r)-> r

(* it adds some assignent to fresh variables if the function is called with
  expressions other than variables
 *)
let rec convert_function_args args=
  match args with
    [] -> ([],[])
  | arg::rest ->
     match arg with
       LVal(va) ->(
       match va with
         Var(name,typ)->
          let (assigns, converted_args)=convert_function_args rest in
          (assigns, name::converted_args)
       | _ ->  let (assigns, converted_args)=convert_function_args rest in
               let r_val= convert_rexpr arg in
               let l_var= get_fresh_variable () in
               (CSIAssign(l_var,r_val)::assigns, l_var::converted_args)
     )
     | _ ->let (assigns, converted_args)=convert_function_args rest in
           let r_val= convert_rexpr arg in
           let l_var= get_fresh_variable () in
           (CSIAssign(l_var,r_val)::assigns, l_var::converted_args)
let convert_function_ret lval=
  match lval with
    Var(variable) -> convert_variable variable
  | _ -> get_fresh_variable ()

let convert_cond cond=
  match cond with
    Cond(rexpr) -> convert_logical_rexpr rexpr
  | _ -> Types.LRandom

let convert_inst (inst : inst) =
  match inst with
    Assign(l,r) ->  [CSIAssign(get_lvalue_var l,convert_rexpr r)]
  | Assume(cond) -> [CSIAssume(convert_cond cond)]
  | Tick(v) -> ( let rval = convert_rexpr v in
                       match desc rval with ENum(tv) -> [CSITick(tv)]
                                     | _ ->raise (Unexpected_value "Found non-constant tick")
                     )
  | Call(rets,name,args) ->
     let (extra_assignments,call_args) = convert_function_args args in
     let converted_rets = List.map convert_function_ret rets in
     (List.append extra_assignments [CSICallArg(converted_rets,name,call_args)])

let cond_add_var locals var=
  if ((String.length var)>10 &&(String.sub var 0 10) = fun_param) then
       (locals:= var::!locals)
    else
      ()
(* add the additional local variables created for function parameters
   and function returns
 *)
let add_extra_locals locals inst =
  match inst with
    CSIAssign(var,r_side) ->cond_add_var locals var
  | CSICallArg(rets,name,call_args)->
     List.iter (fun ret ->cond_add_var locals ret) rets
  | _ -> ()

let create_if_blocks logic_expr succs pred=
  match succs with
    [if_dest;else_dest] ->
    ({ CS_Interop_Types.bpreds = [pred];
       CS_Interop_Types.binsts =  [CSIAssert(logic_expr)];
       CS_Interop_Types.bjump = JJmp([if_dest]);
     },
     { CS_Interop_Types.bpreds = [pred];
       CS_Interop_Types.binsts =  [CSIAssert(negate_logical logic_expr)];
       CS_Interop_Types.bjump = JJmp([else_dest]);
    })
  | _ -> raise (Unexpected_value "branch with condition and more that 2 successors")

let block_has_return {bpreds; binsts; bjump}=
  match bjump with
    JRetVar(_)->true
  | JRet(_)-> true
  | JJmp(_)-> false

let get_blocks_with_return_id blocks=
  let rec get_blocks_with_return_id_aux blocks index=
    match blocks with
      block::blocks_tail -> if(block_has_return block) then
                              index::(get_blocks_with_return_id_aux blocks_tail (index+1))
                            else
                              get_blocks_with_return_id_aux blocks_tail (index+1)
    | [] -> []
  in
  get_blocks_with_return_id_aux blocks 0

let substitute_return_by_jmp return_block_id return_instruction {bpreds;binsts;bjump}=
  if(block_has_return {bpreds;binsts;bjump}) then
    if(bjump=return_instruction) then
      {CS_Interop_Types.bpreds=bpreds;
       CS_Interop_Types.binsts=binsts;
       CS_Interop_Types.bjump=(CS_Interop_Types.JJmp([return_block_id]))
      }
    else
      raise (Unexpected_value "a method returns different things at different locations ")
  else
    {bpreds;binsts;bjump}

let transform_to_single_return block_array=
  let return_blocks= get_blocks_with_return_id (Array.to_list block_array) in
  if (List.length return_blocks)>1 then
    let return_instruction= (Array.get block_array (List.hd return_blocks)).bjump in
     let new_return_block_id=  Array.length block_array in
     Array.append
       (Array.map (substitute_return_by_jmp new_return_block_id return_instruction) block_array)
       (Array.of_list [{ CS_Interop_Types.bpreds = return_blocks;
                         CS_Interop_Types.binsts =  [];
                         CS_Interop_Types.bjump = return_instruction
       }])
  else
    block_array

let convert_funcs cs_func =
  let blist = cs_func.InterIR.fbody in
  current_loc_map := [];
  current_arg_map := [];
  current_arg_map := List.map convert_variable cs_func.InterIR.fargs;
  current_loc_map := List.map convert_variable cs_func.InterIR.flocs;
  (*c4b_func*)
  let block_map = ref [] in
  let convert_blks blk = (
      let cfg_insts = List.map convert_inst blk.InterIR.binsts in
      let updated_list = (List.flatten cfg_insts) in
      (* add the additional local variables *)
      let _ = List.map (add_extra_locals current_loc_map) updated_list in
      let cur_block =
        { CS_Interop_Types.bpreds = [];
          CS_Interop_Types.binsts = updated_list;
          CS_Interop_Types.bjump = JJmp([]);
        } in
      block_map := !block_map @ [cur_block];
    ) in
  Array.iter convert_blks blist;
  let blk_array = Array.of_list !block_map in
  let copy_array = Array.copy blk_array in
  let block_counter= ref (Array.length blk_array) in
  let if_blocks= ref [] in
  let create_branches x blk = (
      let end_point = blk.btype in
      match end_point with
        Return(rets) -> (
        let new_rets = (match rets with
                          [] -> JRet(0)
                        | _ -> JRetVar(List.map convert_variable rets)
                       )
        in
        let current_blk = Array.get copy_array x in
        let new_blk = {current_blk with CS_Interop_Types.bjump = new_rets} in
        Array.set copy_array x new_blk;
      )
      | Branch(children) ->
         (
           let condition = blk.bcond in
           match condition with
             Some(Cond(rexpr)) -> (
             let current_blk = Array.get copy_array x in
             let if_block,else_block=create_if_blocks (convert_cond (Cond rexpr)) children x in
             if_blocks:= !if_blocks @[if_block;else_block];
             let if_id,else_id= (!block_counter, !block_counter+1) in
             let new_c_blk = {current_blk with CS_Interop_Types.bjump = CS_Interop_Types.JJmp([if_id;else_id])} in
             block_counter:=!block_counter+2;
             (* update predecessors *)
             let if_dest =(List.hd children) in
             let t_block = Array.get copy_array if_dest in
             let new_t_preds = t_block.bpreds @ [if_id] in
             let new_t_blk = {t_block with  CS_Interop_Types.bpreds = new_t_preds } in

             let else_dest =(List.hd (List.tl (children))) in
             let f_block = Array.get copy_array else_dest in
             let new_f_preds = f_block.bpreds @ [else_id] in
             let new_f_blk = {f_block with  CS_Interop_Types.bpreds = new_f_preds } in

             Array.set copy_array x new_c_blk;
             Array.set copy_array if_dest new_t_blk;
             Array.set copy_array else_dest new_f_blk;
           )
           | _ -> ( let cur_blk = Array.get copy_array x in
                    (if List.length children > 0 then begin
                         let first = List.hd children in
                         let one_blk = Array.get copy_array first in
                         let one_preds = one_blk.bpreds in
                         let new_one_preds = one_preds @ [x] in
                         let new_one_blk = {one_blk with CS_Interop_Types.bpreds = new_one_preds} in
                         Array.set copy_array first new_one_blk;
                         (if List.length children > 1 then begin
                              let second = List.hd (List.tl children) in
                              let two_blk = Array.get copy_array second in
                              let two_preds = two_blk.bpreds in
                              let new_two_preds = two_preds @ [x] in
                              let new_two_blk = {two_blk with CS_Interop_Types.bpreds = new_two_preds} in
                              Array.set copy_array second new_two_blk;
                            end);
                       end);
                    let new_c_blk = {cur_blk with CS_Interop_Types.bjump = CS_Interop_Types.JJmp(children)} in
                    Array.set copy_array x new_c_blk;
                  )
         )
    ) in
  Array.iteri create_branches blist;
  let new_block_array= Array.append copy_array (Array.of_list !if_blocks) in
  let final_block_array= transform_to_single_return new_block_array in
  let cb_func = 
    {  CS_Interop_Types.fname = cs_func.funname;
       CS_Interop_Types.flocs = !current_loc_map;
       CS_Interop_Types.fargs = !current_arg_map;
       CS_Interop_Types.fbody = final_block_array;
    } in
  cb_func

(* create c4b func *)
let init dump_blk fname =
  let chan = open_in fname in
  let cs_list = Marshal.from_channel chan in
  close_in chan;
  match cs_list with
    [func_list,glos,main] -> (
      glob_map := List.map convert_variable glos;
      let func_l = List.map convert_funcs func_list in 
      (* procs_lst := List.map convert_funcs func_list; *)
      let m_cmp : funcdesc -> bool  = fun s -> (
        let c_val = String.compare s.fname main.funname in
        if c_val = 0 then begin
          true
        end else begin
          false
        end
      ) in
      let new_globals, new_func_l = CS_Utils.transform_callargs dump_blk !glob_map func_l in
      let mf = List.find m_cmp new_func_l in
      glob_map := new_globals;
      procs_lst := new_func_l;
      main_func := mf;
      ()
   )
   | _ -> ()


(* Querier accessor functions *)

let get_glos () = !glob_map

let get_main () = !main_func

let get_func fn =
  List.find (fun x -> x.fname = fn) !procs_lst

let get_args f = f.fargs
let get_locs f = f.flocs
let get_name f = f.fname
let get_nblk f = Array.length f.fbody
let get_blk f n = f.fbody.(n)
let get_nins b = List.length b.binsts
let get_ins b n = List.nth b.binsts n
let get_jmp b = b.bjump
let get_preds b = b.bpreds

