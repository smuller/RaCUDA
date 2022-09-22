(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)

(************ the work flow ****************************
 * parse the input file
 * construct CFG
 * apply weaken rules heuristically [option]
 * do abstract interpretation using simple or apron
 * add focus functions if needed
 * do analysis
 *******************************************************)

open Format
open Types
(* Use the CFG querier for analysis *)
module Cquery_cs = CS_Interop.Make_Graph(Cs_conversion)
(* For testing .cfg programs *)
module Cquery_cfg = CS_Interop.Make_Graph(CS_Querier_CFG)

let input_file = ref ""
let main_func = ref None
let dump_ai = ref false
let dump_stats = ref true
let dump_coq = ref false
let dump_cfg = ref false
let dump_blk = ref false
let dump_neededness = ref false
let no_weaken = ref false
let no_focus = ref false
let degree = ref 2
let ascii = ref true
let ai = ref "simple"
let analysis_mode = ref "size-change"
let no_sampling_transformation = ref false
let analysis_type = ref Analysis.Size_change
let implicit_main = ref None
let m_mode = ref false
let neededness = ref false
let eval = ref false
let param_file = ref None
let cuda_metric = ref None
let output_opt = ref None

let usagemsg = Printf.sprintf "usage: %s [OPTIONS] FILE/INPUT_CHANNEL\n" Sys.argv.(0)
let argspec = Arg.align

  [ "-func", Arg.String (fun s -> main_func := Some s),
    "<fn> Analyze the specified function"
  ; "-ai", Arg.Symbol (["apron"; "simple"], fun s -> ai := s),
    " Select the abstract interpretation backend"

  ; "-analysis-mode", Arg.Symbol (["size-change"; "water-mark"], fun s -> analysis_mode := s), 
    " Select the analysis mode"

  ; "-ascii", Arg.Set ascii,
    " Output results using ascii only"

  ; "-dump-ai", Arg.Set dump_ai,
    " Display abstract interpretation results"
  
  ; "-dump-stats", Arg.Set dump_stats,
    " Display statistics of the analysis"

  ; "-dump-coq", Arg.Set dump_coq,
    " Generate a Coq proof"

  ; "-degree", Arg.Int (fun d -> degree := d),
    " Maximum degree to consider in the bound search"

  ; "-no-weaken", Arg.Set no_weaken,
    " Do not automatically add weakening points"

  ; "-no-transform", Arg.Set no_sampling_transformation,
    " Do not automatically transform samplings to probabilistic branchings"

  ; "-no-focus", Arg.Set no_focus,
    " Do not automatically add focus functions"
  
  ; "-m", Arg.Set m_mode, 
    " Analyze the program in module mode"

  ; "-dump-cfg", Arg.Set dump_cfg,
    " Output the generated CFG in file graph_prog.cfg"

  ; "-dump-blk", Arg.Set dump_blk,
    " Output the generated block format in file blk_prog.cfg"

  ; "-dump-neededness", Arg.Set dump_neededness,
    " Output the neededness analysis"

  ; "-neededness", Arg.Set neededness, 
    " Analyze the program with neededness analysis"

  ; "-eval", Arg.Set eval,
    " Evaluate the CUDA code with the given metric"

  ; "-param-file", Arg.String (fun s -> param_file := Some s),
    "<file> use <file> as parameters for CUDA analysis or evaluation"

  ; "-metric", Arg.Symbol (["steps"; "mem"; "divwarps";
                            "global"; "shared"; "debug"],
                           ((fun f s -> cuda_metric := Some (f s))
                              (let open CUDA_Cost in
                               (function "steps" -> cmetric_steps
                                       | "mem" -> cmetric_memaccesses
                                       | "divwarps" -> cmetric_divwarps
                                       | "debug" -> cmetric_debug
                                       | "global" -> cmetric_global
                                       | "shared" -> cmetric_shared)))),
    " specify the cost metric for CUDA analysis or evaluation"

  ; "-opt", Arg.String (fun s -> output_opt := Some s),
    "<file> Optimize kernel and output to <file>"
  ]

let annonarg s =
  if !input_file <> "" then
    raise (Arg.Bad "too many input files");
  input_file := s

(* print the error and exit *)
let failarg msg =
  Printf.eprintf "%s: %s\n" Sys.argv.(0) msg;
  Arg.usage argspec usagemsg;
  exit 1

let exec_llvm_reader f =
  let reader = "/llvm-reader" in
  let candidates =
    [ Config.build_path ^ "/../llvm-reader" ^ reader
    ] in
  match
    List.fold_left (fun ico cand ->
      if ico = None then
        try
          let _ = Unix.access cand [Unix.X_OK] in
          let cmd = Printf.sprintf "%s %s" cand f in
          Some (Unix.open_process_in cmd)
        with Sys_error _ | Unix.Unix_error _ -> None
      else ico
    ) None candidates
  with
  | Some ic -> ic
  | None ->
    Format.eprintf "%s: llvm-reader could not be found or run@."
      Sys.argv.(0);
    raise Utils.Error

let ends_with s s' =
  let ls' = String.length s' and ls = String.length s in
  ls' >= ls && String.sub s' (ls' - ls) ls = s

(* entry point *)
let entry () = 
  let total_runtime = ref 0.0 in
  let tick_var = "tick_z" in
  let generated_proof_name = "generated_coq.v" in
  Arg.parse argspec annonarg usagemsg;
  
  (* set analysis mode *)
  let analysis_type = match !analysis_mode with
    | "size-change" -> ref Analysis.Size_change
    | "water-mark" -> ref Analysis.Water_mark
    | m -> failarg (Printf.sprintf "%s mode is not supported" m)
  in

  (* analysis function *)
  let main_analyze () = 
    let params = ref None in

    (* let globals, g_funcl = try *)
    let progs_to_analyze = try
      (* CFG: if input is in block format *)
      if !input_file = "" then
        begin
          Cquery_cfg.init !dump_blk !input_file;
          let globals = Cquery_cfg.get_glos () in
          let mainf_name, g_cfg = Cquery_cfg.graph_from_main (not !no_sampling_transformation) tick_var in
          implicit_main := Some mainf_name;
          [Utils.add_tick_var tick_var globals, g_cfg, None]
        end
      (* CFG: if input file is in CS format *)
      else if ends_with ".cs" !input_file then
        begin
          Cquery_cs.init !dump_blk !input_file;
          let globals = Cquery_cs.get_glos () in
          let mainf_name, g_cfg = Cquery_cs.graph_from_main (not !no_sampling_transformation) tick_var in
          implicit_main := Some mainf_name;
          [Utils.add_tick_var tick_var globals, g_cfg, None]
        end
      (* CFG: if input file is in IMP format *)
      else if ends_with ".imp" !input_file then
        let globals, imp_file = IMP.parse_file !input_file in
        (* transform function calls with arguments and return values *)
        let globals, imp_file = IMP.function_call_transformation (fun () -> ()) globals imp_file in
        [Utils.add_tick_var tick_var globals, 
         List.map (fun imp_f -> Graph.from_imp (not !no_sampling_transformation) tick_var imp_f) imp_file,
        None]
     (* CFG: if input fule is in CUDA-C format *)
      else if ends_with ".cu" !input_file then
        match Frontc.parse_file !input_file stdout with
        | Frontc.PARSING_ERROR -> failwith "parse error"
        | Frontc.PARSING_OK ccode ->
           let cuda =
             CUDA.cuda_of_file (fun () -> ()) !input_file ccode in
           let p =
             (match !param_file with
              | Some s -> let (p, _, _) = CUDA_Params.params_of_file s in p
              | None -> CUDA_Params.default_params)
           in
           let m = (match !cuda_metric with
                    | Some m -> m
                    | None -> failwith "no metric specified")
           in
           let _ = params := Some (p, m) in
           let ret_cuda_prog cuda =
             let (globals, imp_file) =
               CUDA_Cost.imp_of_prog p m cuda
             in
             (* let _ = IMP_Print.print_prog Format.std_formatter
                (globals, imp_file) in *)
             (* transform function calls with arguments and return values *)
             let globals, imp_file = IMP.function_call_transformation (fun () -> ()) globals imp_file in
             Utils.add_tick_var tick_var globals, 
             List.map (fun imp_f -> Graph.from_imp (not !no_sampling_transformation) tick_var imp_f) imp_file,
             Some cuda
           in
           [ret_cuda_prog cuda]
           
      (* CFG: if input file is in LLVM bit-code format *)
      else if ends_with ".o" !input_file || ends_with ".bc" !input_file then
        let ic = exec_llvm_reader !input_file in
        begin 
          try
            let gfunc = Graph_Reader.read_func ic in
            let gfunc = Graph.add_loop_counter tick_var gfunc in
            [[], [gfunc], None]
          with End_of_file ->
            match Unix.close_process_in ic with
            | Unix.WEXITED 0 -> 
              failwith "llvm-reader should have failed"
            | Unix.WEXITED _ -> 
              Format.eprintf "%s: llvm-reader could not parse '%s'@." Sys.argv.(0) !input_file;
              raise Utils.Error
            | _ ->
              Format.eprintf "%s: llvm-reader process was killed@." Sys.argv.(0);
              raise Utils.Error
        end
      else 
        begin
          Format.eprintf "%s: unknown input file type for '%s'@." Sys.argv.(0) !input_file;
          raise Utils.Error
        end
    with Sys_error _ ->
      Format.eprintf "%s: cannot open file '%s'@." Sys.argv.(0) !input_file;
      raise Utils.Error
    in

    let analyze_prog (globals, g_funcl) =
    (* for testing: the CFG *)
      if !dump_cfg then
        begin
          let oc = open_out "graph_prog.cfg" in 
          Graph.print_graph_prg oc globals g_funcl;
          close_out oc
        end;
      
      (* get the start function *)
      let fstart =
        match !main_func with
        | Some f -> f
        | None ->
           if List.length g_funcl = 1 then 
             let f = List.hd g_funcl in 
             f.fun_name
           else 
             begin
               match !implicit_main with
               | Some f -> f 
               | None -> "start"
             end
      in
      
      (* check that the existance of the main function *)
      if not (List.exists (fun f -> f.Types.fun_name = fstart) g_funcl) then 
        failarg (Printf.sprintf "cannot find function '%s' to analyze" fstart);
      
      (* add weaken heuristically *)
      let g_funcl =
        if !no_weaken then g_funcl else
          List.map Heuristics.add_weaken g_funcl
      in
      
      (* for testing: the CFG with weakennings *)
      if !dump_cfg then
        begin
          let oc = open_out "graph_prog_weaken.cfg" in 
          Graph.print_graph_prg oc globals g_funcl;
          close_out oc
        end;
      
      (* topology order the CFGs *)
      let g_funcl = List.map Graph.rpo_order g_funcl in

      (* generate an AI *)
      let module AI = (val begin
                         match !ai with
                         (* | "apron" -> (module Graph.AbsInt.Apron) *)
                         | _       -> (module Graph.AbsInt.Simple)
                                        end: Graph.AbsInt)
      in

      (* analyze a function *)
      let analyze_fun f_name =
        (* performing AI *)
        let ai_results =
          AI.analyze ~dump:!dump_ai (!params) (globals, g_funcl) f_name in

        let query =
          let open Polynom in
          (Poly.of_monom (Monom.of_var tick_var) (+1.))
        in

        (* For each degree from 1 to !degree, heuristically add focus functions and 
         * compute the result until it finds a valid result. Thus, if users give very big 
         * degree (e.g., 100) but in fact the bound has 1 degree then try_run only run 
         * 1 iteration
         *)
        let g_funcl, st_results =
          let rec try_run d =
            if d > !degree then 
              (g_funcl, None) 
            else 
              begin
                let g_funcl = 
                  if !no_focus then 
                    g_funcl
                  else
                    (* Heuristic.add_focus needs to be checked, it seems to run forever for some special functions *)
                    g_funcl 
                    |> List.map (Heuristics.add_focus ~degree:d ai_results AI.get_nonneg AI.is_nonneg)
                    |> List.map (Heuristics.add_focus_old ~degree:d ai_results AI.get_nonneg AI.is_nonneg)
                in
                match
                  Analysis.run ai_results AI.is_bot AI.is_nonneg (globals, g_funcl) f_name d !analysis_type query tick_var !neededness !dump_neededness
                with
                | None -> try_run (d+1)
                | Some sol -> (g_funcl, Some sol)
              end
          in try_run 1
        in

        let poly_print =
          if !ascii then Polynom.Poly.print_ascii else Polynom.Poly.print
        in

        (* print the result *)
        printf "@.%s:@.    @[<v>" f_name;

        (match st_results with
        | None ->
           printf "Sorry, I could not find a bound@ "
        | Some (annots, p) -> 
           begin
             match !analysis_type with
             | Analysis.Water_mark -> 
                printf "Bound: %a@ " poly_print (CUDA.lookup_poly
                                                   (Polynom.Poly.sub p query))
             | Analysis.Size_change -> 
                printf "Bound: %a@ " poly_print (CUDA.lookup_poly
                                                   (Polynom.Poly.sub p query))
           end;
           Format.printf "Degree: %d@ " (Polynom.Poly.degree p);

           (* print statistic information *)
           if !dump_stats then 
             begin
               let { Analysis.num_lpvars; num_lpcons; max_focus; lp_runtime } = Analysis.stats in
               printf "Number of LP variables: %d@ " num_lpvars;
               printf "Number of LP constraints: %d@ " num_lpcons;
               printf "Maximum focus functions in use: %d@ " max_focus;
               printf "LP solver time: %.3fs@ " !lp_runtime
             end;
           printf "@]@.";

           (* generate Coq proof *)
           if !dump_coq then
             begin
               let generated_proof_name = f_name ^ "_" ^ generated_proof_name in
               try
                 Coqgen.dump generated_proof_name f_name (globals, g_funcl) query p AI.print_as_coq ai_results annots
               with 
                 Utils.Todo what ->
                 begin
                   if Sys.file_exists generated_proof_name then
                     Sys.remove generated_proof_name;
                   Format.eprintf "Coq extraction failure (%s)@." what
                 end
             end);
        st_results
    in 

    (* if module mode then analyze all functions *)
    if !m_mode then 
      begin
        List.fold_left 
          (fun _ f -> 
            let fn = f.fun_name in
            analyze_fun fn
          ) None g_funcl;
      end
    (* else analyze only the main function *) 
    else
      analyze_fun fstart
    in
    let best =
      List.fold_left
        (fun best (globals, g_funcs, Some cuda) ->
          Format.fprintf Format.std_formatter "Analyzing:";
          (* CUDA.print_cprog Format.std_formatter cuda; *)
          match (analyze_prog (globals, g_funcs), best) with
          | (None, _) -> None
          | (Some (annot, p), None) -> Some (p, cuda)
          | (Some (_, p), Some (best_poly, best_prog)) ->
             if Polynom.Poly.always_less p best_poly then
               Some (p, cuda)
             else
               best
        )
        None
        progs_to_analyze
    in
    match best with
    | None -> failwith "impossible"
    | Some (_, cuda) ->
       (match !output_opt with
        | None -> 0
        | Some file ->
           let outc = open_out file in
           let fmt = Format.formatter_of_out_channel outc in
           let fmtt = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
           CUDA.print_cprog fmtt cuda;
           close_out outc;
           0)
    | _ -> failwith "impossible"
  in
  if !eval then
    if ends_with ".cu" !input_file then
      match Frontc.parse_file !input_file stdout with
      | Frontc.PARSING_ERROR -> failwith "parse error"
      | Frontc.PARSING_OK ccode ->
         let cuda = CUDA.cuda_of_file (fun () -> ()) !input_file ccode in
         let (p, varmap, heap) =
           match !param_file with
           | Some s -> CUDA_Params.params_of_file s
           | None -> (CUDA_Params.default_params,
                      CUDA_Params.default_map,
                      CUDA_Types.Heap.empty)
         in
         let tid_of_idx d =
           let open CUDA_Cost in
           d.x + d.y * p.blockdim.x + d.z * p.blockdim.x * p.blockdim.y
         in
         let idx_of_tid tid =
           let open CUDA_Cost in
           let x = tid mod p.blockdim.x in
           let y = (tid / p.blockdim.x) mod p.blockdim.y in
           let z = tid / (p.blockdim.x * p.blockdim.y)
           in
           {x = x;
            y = y;
            z = z}
         in
         let m = (match !cuda_metric with
                    | Some m -> m
                    | None -> failwith "no metric specified")
         in
         (match cuda with
          | (_, [f]) ->
             let open CUDA_Cost in
             let (_, cost) =
               CUDA_Eval.eval_cfunc p varmap heap m
                 (List.init (p.warp_size)
                    (fun i ->
                      let (ix, iy, iz) = (match p.init_thread with
                                          | Some {x = ix; y = iy; z = iz} ->
                                             (ix, iy, iz)
                                          | None -> (0, 0, 0))
                      in
                      {x = ix + (i mod p.blockdim.x);
                       y = iy + ((i / p.blockdim.x) mod p.blockdim.y);
                       z = iz + (i / (p.blockdim.x * p.blockdim.y))}))
                 f
             in
             (printf "Evaluation cost: %d\n" cost; 0)
          | _ -> failwith "File for evaluation must contain only one function")
    else
      failwith "Can only evaluate CUDA"
  else
  (* measure the total runtime *)
  try 
    let retcode = Time.wrap_duration total_runtime main_analyze in
    (* print statistic information for the analysis of the whole program *)
    if !dump_stats then 
      begin
        printf "@.Program Statistics:@.    @[<v>";
        (*
        let { Graph.weaken_map } = Graph.stats in
        if not !no_weaken then 
          begin
            printf "Weakenings inserted per function:@     @[<hov>";
            let first = ref true in
            List.iter (fun (f, n) -> printf (if !first then "%d for %s" else ",@ %d for %s") n f;
            first := false) (List.rev weaken_map);
            printf "@]@ ";
          end;
        *)
        printf "Total runtime: %.3fs" !total_runtime;
        printf "@]@.";
      end;
    retcode
  with Utils.Error -> 2

(* begin from here *)
let () = Utils.exit (entry ())
