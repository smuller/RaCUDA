(* Van Chan Ngo - 2017 *)

open Types
open Dist
open Graph_Types
open CS_Interop_Types

module Make_Graph(Q: CS_Interop_Types.CS_Querier) = 
struct

  include Q

  let is_fassert_prob blk = 
    try 
      match get_ins blk 0 with
      | CSIAssert _ | CSProbIf _ -> true
      | _ -> false
    with Failure _ -> false

  let blk_foldr f bd x =
    let i0 = 
      if is_fassert_prob bd then 1 else 0
    in 

    let n = get_nins bd in
    let x = ref x in
    for i = (n - 1) downto i0 do
      x := f !x (get_ins bd i);
    done;
    !x

  let blk_foldl f bd x = 
    let i0 = 
      if is_fassert_prob bd then 1 else 0
    in 

    let n = get_nins bd in
    let x = ref x in
    for i = i0 to (n - 1) do
      x := f !x (get_ins bd i);
    done;
    !x

  let fun_iteri f fd =
    let n = get_nblk fd in
    for i = 0 to (n - 1) do
      f i (get_blk fd i);
    done

  (*
   * make the graph representation of a cfg_func
   * a block id is the initial node of the sub-graph 
   * for this block.
   * the edges are instructions in the blocks.
   * note that the function blocks are in DFS order.
   *)

  let graph_from_fundesc transformed tick_var cfg_func =
    let h_edges = Hashtbl.create 45 in
    let initialnumber_of_nodes = get_nblk cfg_func in
    if false then 
      Printf.printf "number of blocks: %d\n" initialnumber_of_nodes;
    let next_node = ref initialnumber_of_nodes in
    let start_node = 0 in
    let end_node = ref 0 in
    let name = get_name cfg_func in
    let locs = get_locs cfg_func in
    let focus = [] in

    let new_node () =
      let node = !next_node in
      incr next_node;
      node
    in

    let new_edge src act dst =
      if dst < 0 then
        failwith "invalid input program";
      Hashtbl.add h_edges src (act, dst)
    in

    let create_assign_edge id e s_node = 
      let e_node = new_node () in
      new_edge s_node (AAssign (id, ref e, ref false)) e_node; 
      e_node
    in

    (*
    let create_random_edge id e s_node = 
      let e_node = new_node () in
      new_edge s_node (ANone) e_node;
      e_node
    in
    *)
    
    (* get the first instruction of a block and check that it is an assert *)
    let get_first_assert blk = 
      try 
        match get_ins blk 0 with
        | CSIAssert log -> Some log
        | _ -> None
      with Failure _ -> 
        None 
    in

    (* get the first instruction of a block and check that it is a probabilistic branching *)
    let get_first_prob blk = 
      try 
        match get_ins blk 0 with
        | CSProbIf (EProb (a, b)) -> Some (a, b)
        | _ -> None
      with Failure _ -> 
        None 
    in
    
    let to_prob_branching id e op s_node keys probs =
      let last_key = List.hd (List.rev keys) in
      let keys' = List.rev (List.tl (List.rev keys)) in
      let e_node = new_node () in

      let create_prob_branching n (k, p) = 
        let ln' = new_node () in
        let rn' = new_node () in
        new_edge n (AProb p) ln';
        (* edge to fin *)
        let e' = match op with
                 | Dadd ->
                   EAdd (e, ENum k)
                 | Dsub ->
                   ESub (e, ENum k)
                 | Dmul ->
                   EMul (e, ENum k)
        in
        new_edge ln' (AAssign (id, ref e', ref false)) e_node;
        new_edge n (AProb (1. -. p)) rn';
        rn'
      in

      let last_level_node = List.fold_left create_prob_branching s_node (List.combine keys' probs) in
      (* the last edge *)
      let e' = match op with
               | Dadd ->
                 EAdd (e, ENum last_key)
               | Dsub ->
                 ESub (e, ENum last_key)
               | Dmul ->
                 EMul (e, ENum last_key)
      in
      new_edge last_level_node (AAssign (id, ref e', ref false)) e_node;
      e_node
    in

    let transform_ber id e op d s_node =
      match d with
      | EBer (pa, pb) ->
        let (keys, probs) = List.split (Dist.ber_to_list pa pb) in
        to_prob_branching id e op s_node keys [List.hd probs]
      | _ -> failwith "invalid input program"
    in
  
    let transform_bin id e op d s_node = 
      match d with
      | EBin (n, pa, pb) -> 
        let key_probs = Dist.bin_to_list n pa pb in 
        let transformed_probs = Dist.transform_prob key_probs in 
        to_prob_branching id e op s_node (fst (List.split key_probs)) transformed_probs
      | _ -> failwith "invalid input program"
    in

    let transform_geo id e op d s_node = 
      failwith "geometric sampling distribution is not supported"
    in

    let transform_nbin id e op d s_node = 
      failwith "negative binomial sampling distribution is not supported"
    in

    let transform_pois id e op d s_node =
      failwith "poisson sampling distribution is not supported"
    in

    let transform_hyper id e op d s_node = 
      match d with
      | EHyper (n, r, m) ->
        let key_probs = Dist.hyper_to_list n r m in 
        let transformed_probs = Dist.transform_prob key_probs in
        to_prob_branching id e op s_node (fst (List.split key_probs)) transformed_probs
      | _ -> failwith "invalid input program"
    in

    let transform_unif id e op d s_node = 
      match d with
      | EUnif (lb, ub) ->
        let key_probs = Dist.unif_to_list lb ub in
        let transformed_probs = Dist.transform_prob key_probs in
        to_prob_branching id e op s_node (fst (List.split key_probs)) transformed_probs
      | _ -> failwith "invalid input program"
    in

    let transform_dist id e s_node =
      match e with
      | EAdd (e1, e2) ->
        let dt1 = Dist.get_type e1 in
        let dt2 = Dist.get_type e2 in
        begin
          match (dt1, dt2) with
          | (Det, Det) ->
            create_assign_edge id e s_node
          | (Det, Ber) -> 
            transform_ber id e1 Dadd e2 s_node
          | (Det, Bin) ->
            transform_bin id e1 Dadd e2 s_node
          | (Det, Geo) ->
            transform_geo id e1 Dadd e2 s_node
          | (Det, Nbin) ->
            transform_nbin id e1 Dadd e2 s_node
          | (Det, Pois) ->
            transform_pois id e1 Dadd e2 s_node
          | (Det, Hyper) ->
            transform_hyper id e1 Dadd e2 s_node
          | (Det, Unif) ->
            transform_unif id e1 Dadd e2 s_node
          | (Ber, Det) -> 
            transform_ber id e2 Dadd e1 s_node
          | (Bin, Det) ->
            transform_bin id e2 Dadd e1 s_node
          | (Geo, Det) ->
            transform_geo id e2 Dadd e1 s_node
          | (Nbin, Det) ->
            transform_nbin id e2 Dadd e1 s_node
          | (Pois, Det) ->
            transform_pois id e2 Dadd e1 s_node
          | (Hyper, Det) ->
            transform_hyper id e2 Dadd e1 s_node
          | (Unif, Det) ->
            transform_unif id e2 Dadd e1 s_node
          | (_, _) ->
            failwith "invalid input program"
        end
      | ESub (e1, e2) ->
        let dt1 = Dist.get_type e1 in
        let dt2 = Dist.get_type e2 in
        begin
          match (dt1, dt2) with
          | (Det, Det) ->
            create_assign_edge id e s_node
          | (Det, Ber) -> 
            transform_ber id e1 Dsub e2 s_node
          | (Det, Bin) ->
            transform_bin id e1 Dsub e2 s_node
          | (Det, Geo) ->
            transform_geo id e1 Dsub e2 s_node
          | (Det, Nbin) ->
            transform_nbin id e1 Dsub e2 s_node
          | (Det, Pois) ->
            transform_pois id e1 Dsub e2 s_node
          | (Det, Hyper) ->
            transform_hyper id e1 Dsub e2 s_node
          | (Det, Unif) ->
            transform_unif id e1 Dsub e2 s_node
          | (Ber, Det) -> 
            transform_ber id e2 Dsub e1 s_node
          | (Bin, Det) ->
            transform_bin id e2 Dsub e1 s_node
          | (Geo, Det) ->
            transform_geo id e2 Dsub e1 s_node
          | (Nbin, Det) ->
            transform_nbin id e2 Dsub e1 s_node
          | (Pois, Det) ->
            transform_pois id e2 Dsub e1 s_node
          | (Hyper, Det) ->
            transform_hyper id e2 Dsub e1 s_node
          | (Unif, Det) ->
            transform_unif id e2 Dsub e1 s_node
          | (_, _) ->
            failwith "invalid input program"
        end
      | EMul (e1, e2) ->
        let dt1 = Dist.get_type e1 in
        let dt2 = Dist.get_type e2 in
        begin
          match (dt1, dt2) with
          | (Det, Det) ->
            create_assign_edge id e s_node
          | (Det, Ber) -> 
            transform_ber id e1 Dmul e2 s_node
          | (Det, Bin) ->
            transform_bin id e1 Dmul e2 s_node
          | (Det, Geo) ->
            transform_geo id e1 Dmul e2 s_node
          | (Det, Nbin) ->
            transform_nbin id e1 Dmul e2 s_node
          | (Det, Pois) ->
            transform_pois id e1 Dmul e2 s_node
          | (Det, Hyper) ->
            transform_hyper id e1 Dmul e2 s_node
          | (Det, Unif) ->
            transform_unif id e1 Dmul e2 s_node
          | (Ber, Det) -> 
            transform_ber id e2 Dmul e1 s_node
          | (Bin, Det) ->
            transform_bin id e2 Dmul e1 s_node
          | (Geo, Det) ->
            transform_geo id e2 Dmul e1 s_node
          | (Nbin, Det) ->
            transform_nbin id e2 Dmul e1 s_node
          | (Pois, Det) ->
            transform_pois id e2 Dmul e1 s_node
          | (Hyper, Det) ->
            transform_hyper id e2 Dmul e1 s_node
          | (Unif, Det) ->
            transform_unif id e2 Dmul e1 s_node
          | (_, _) ->
            failwith "invalid input program"
        end
      | _ ->
        failwith "it should not happen"
    in

    (* translation of an instruction 
     * s_node: starting node
     * e_node: return end node
     *)
    let do_ins s_node ins =
      match ins with
      | CSITick n -> 
        create_assign_edge tick_var (EAdd (EVar tick_var, ENum n)) s_node

      | CSIAssume log ->
        let e_node = new_node () in
        new_edge s_node (AGuard log) e_node;
        e_node

      | CSIAssign (id, e) ->
        (* transform all samplings into the equivalent probabilistic branchings *)
        if transformed then
          begin
             match e with
            | ERandom ->
              (*failwith "expression contains random"*)
	            create_assign_edge id e s_node
              (*create_random_edge id e s_node*)
            | EVar _ | ENum _ | EDist _ -> 
              create_assign_edge id e s_node
            | EBer _ | EBin _ | EGeo _ | ENbin _ | EPois _ | EHyper _ | EUnif _ -> 
              transform_dist id (EAdd ((ENum 0), e)) s_node
            | EAdd _ | ESub _ | EMul _ -> 
              if (Dist.is_sampling e) then 
                transform_dist id e s_node
              else 
                create_assign_edge id e s_node
          end
        else
          create_assign_edge id e s_node

      | CSICall id -> 
        let e_node = new_node () in
        new_edge s_node (ACall id) e_node;
        e_node

      | CSICallArg _ -> 
        failwith "invalid input program: Call with arguments and return"

      | CSProbIf pr -> 
        failwith "invalid input program: Probabilistic branching must be the first instruction"

      | CSIAssert _ -> 
        failwith "invalid input program: Assertion must be the first instruction"
    in

    (* translation of a block 
     * s_node: the block id and is the starting node
     *)
    let do_blk s_node blk =
      let e_node = blk_foldl do_ins blk s_node in
  
      (* jump processing *)
      let jump_ins = get_jmp blk in
      match jump_ins with
      | JJmp bl ->
        if (List.length bl) = 0 then failwith "invalid input program: Jump without destinations";
        List.iter 
          (fun id_blk -> 
            let blk = get_blk cfg_func id_blk in
            let fassert = get_first_assert blk in 
            match fassert with
            | Some log -> 
              new_edge e_node (AGuard log) id_blk
            | None -> 
              begin
                let fprob = get_first_prob blk in
                match fprob with
                | Some (a, b) -> 
                  let p = (float_of_int a) /. (float_of_int b) in
                  new_edge e_node (AProb p) id_blk
                | None ->
                  new_edge e_node ANone id_blk
              end
          ) bl
      (* end of program *)
      | JRet n ->
        end_node := e_node; ()
      | JRetVar _ -> failwith "invalid input program: Return value"
    in

    fun_iteri do_blk cfg_func;

    let edges = Array.make !next_node [] in
    for i = 0 to !next_node - 1 do
      edges.(i) <- Hashtbl.find_all h_edges i
    done;

    { fun_name = name;
      fun_vars = locs;
      fun_focus = focus;
      fun_body =
        { g_start = start_node;
          g_end = !end_node;
          g_edges = edges;
          g_position = Array.map (fun _ -> Utils.dummy_position) edges
        };
      fun_start_p = Utils.dummy_position;
      fun_end_p = Utils.dummy_position;
      fun_args = []
    }

  (* translation of main function and all its callee functions *)
  let graph_from_main transformed tick_var =
    let h_funcs = Hashtbl.create 45 in
    let add_new_func fname fdesc =
      Hashtbl.replace h_funcs fname fdesc
    in
    (* get all callee functions *)
    let rec get_all_callees fd =
      let do_f_ins ins =
        match ins with
        | CSICall id ->
           let fdesc = get_func id in
           add_new_func id fdesc;
           get_all_callees fdesc
        | _ -> ()
      in

      let do_f_blk blk =
        let n = get_nins blk in
        for i = 0 to n - 1 do
          do_f_ins (get_ins blk i)
        done
      in

      let n = get_nblk fd in
      for i = 0 to n - 1 do
        do_f_blk (get_blk fd i)
      done
    in

    let mainf = get_main () in
    let main_name = (get_name mainf) in
    
    get_all_callees mainf;
    add_new_func main_name mainf;

    let g_prog = List.map (graph_from_fundesc transformed tick_var)
      (Hashtbl.fold
        (fun fn fb acc -> fb :: acc)
        h_funcs []
      )
    in
    (main_name, g_prog)

end
