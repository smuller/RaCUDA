(* Van Chan Ngo - 2017 *)

open Types
include Dist_Types

let fsmall = 1e-7


(*
    get the distribution type represented by e
    e can not be nested.
*)
let get_type e = 
    match desc e with
    | ERandom | EVar _ | ENum _ | EDist _ -> Det
    | EBer _ -> Ber
    | EBin _ -> Bin
    | EGeo _ -> Geo
    | ENbin _ -> Nbin
    | EPois _ -> Pois
    | EHyper _ -> Hyper
    | EUnif _ -> Unif
    | EAdd _ | ESub _ | EMul _ -> failwith "Dist:get_type: invalid input program"


let rec is_sampling e = 
    match desc e with
    | ERandom | EVar _ | ENum _ | EDist _ -> false
    | EBer _ | EBin _ | EGeo _ | ENbin _ | EPois _ | EHyper _ | EUnif _ -> true
    | EAdd (e1, e2) -> (is_sampling e1) || (is_sampling e2)
    | ESub (e1, e2) -> (is_sampling e1) || (is_sampling e2)
    | EMul (e1, e2) -> (is_sampling e1) || (is_sampling e2)
    
(*
    get the gsl_caller for computing probabilities.
    dt : type of distribution
    l : list of parameters.
*)
let exec_gsl_caller dt l =
  let caller = "/gsl_caller" in
  let exe_paths = [Config.build_path ^ "/../gsl_caller" ^ caller] in 
  match 
    List.fold_left 
    begin
        fun ic_is_open exe_path ->
        if ic_is_open = None then
            try
                (* check execution permission *)
                let _ = Unix.access exe_path [Unix.X_OK] in
                (* generate command line with arguments *)
                let parameters = List.fold_left 
                                 (
                                    fun current_p next_p -> 
                                    Printf.sprintf "%s %s" current_p next_p
                                 ) "" l
                in
                let cmd = Printf.sprintf "%s %s %s" exe_path dt parameters in 
                Some (Unix.open_process_in cmd)
            with Sys_error _ | Unix.Unix_error _ -> None
        else ic_is_open
    end None exe_paths
  with
  | Some ic -> 
    ic
  | None -> 
    Format.eprintf "%s: gls_caller could not be found or run@." Sys.argv.(0);
    raise Utils.Error


(*
    read from gsl_caller outputs.
    return a hash table of sampled values and their probabilities.
*)
let read_from_gsl_caller n dt parameters = 
    let ic = exec_gsl_caller dt parameters in 
    (* read the outputs *)
    let probs = ref [] in 
    try
        while true; do
            let p = input_line ic in 
            probs := p :: !probs
        done;
        List.map (fun p -> float_of_string p) (List.rev !probs)
    with End_of_file ->
        match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> 
        begin
            (* return the list of random values with probabilities *)
            if (List.length !probs) != n then 
                failwith "gsl_caller should have failed"
            else
                List.map (fun p -> float_of_string p) (List.rev !probs)
        end
        | Unix.WEXITED _ -> Format.eprintf "%s: gsl_caller could not parse@." Sys.argv.(0); raise Utils.Error
        | _ -> Format.eprintf "%s: gsl_caller process was killed@." Sys.argv.(0); raise Utils.Error


(*
    normalize probabilities and ensure that the sum is 1.
*)
let normalize_prob l = 
    let sum_without_first l = 
        match l with
        | [] -> 0.
        | x::[] -> 0.
        | x::xs -> List.fold_left (fun s p -> s +. p) 0. xs
    in

    let l' = List.map (fun p -> 
                        if p < fsmall then 0.
                        else if (1. -. p) < fsmall then 1.
                        else p
                      ) l
    in
    (* ensure the sum is 1 *)
    let new_last = 1. -. (sum_without_first (List.rev l')) in
    if false then Printf.printf "new_last %f" new_last;
    try
        let l1 = List.tl (List.rev l') in
        List.rev (new_last :: l1)
    with
    | _ -> l'

(*
    transform a list of random values with probabilities 
    (e.g., a bounded probability distribution) into 
    probabilistic branching with 2 branches.

    For example, [(0, 0.5), (1, 0.1), (2, 0.04), (3, 0.36)] 
    where a pair (v, p) means the value v has probability p, 
    can be transformed into:
    
         (0)
       /
  0.5 / 
     /    
    *        (1)
     \     / 
  0.5 \   / 0.2
       \ /
        *        (2)
         \     /
     0.8  \   / 0.1
           \ / 
            *   
             \
         0.9  \
               \
                (3)   

    This can be represented by a list [0.5, 0.2, 0.1] which 
    specifies the left branch's probabilities of all levels 
    of the nested probabilistic branching.

    In general, the list above is computed as follows.

    [p'1, p'2,..., p'(n-1)] = 
    [p1, p2/(1-p'1), p3/(1-p'1)*(1-p'2),..., p(n-1)/(1-p'1)*...*(1-p'(n-2))]
*)
let transform_prob l =
    let compute_prob l = 
        let branch_prob l p = 
            match l with
            | [] -> [p]
            | _ -> 
                let sum = List.fold_left 
                (
                    fun s p -> s *. (1. -. p)
                ) 1. l in
                let new_p = if sum != 0. then p /. sum else p in
                new_p :: l
        in

        List.rev (List.fold_left branch_prob [] l)
    in
    compute_prob (List.rev (List.tl (List.rev (snd (List.split l)))))


(*
    return the list of random values with probabilities 
*)
let unif_to_list lb ub = 
    if (ub < lb) then
        failwith "Dist:unif_to_list: invalid input program"
    else
        begin
            let l = ref [] in 
            let p = 1. /. (float_of_int (ub - lb + 1)) in 
            for i = lb to ub do 
                l := (i, p) :: !l
            done;
            List.rev !l
            (*List.combine (fst (List.split (List.rev !l))) (normalize_prob (snd (List.split (List.rev !l))))*)
        end


(*
    return the list of random values with probabilities
*)
let ber_to_list pa pb = 
    let p = 
    if (pa > 0 && pb < 0) || (pa < 0 && pb > 0) then
        failwith "Dist:ber_to_list: invalid input program"
    else if (pa = 0) then
        0.
    else
        (float_of_int pa) /. (float_of_int pb) in

    [(0, 1. -. p); (1, p)]


(*
    return the list of random values with probabilities 
    using GSL C library
*)
let bin_to_list n pa pb = 
    let bin_type = "1" in 
    let p = (float_of_int pa) /. (float_of_int pb) in 
    let parameters = [(string_of_int n); (string_of_float p)] in 
    (* read the outputs *)
    List.mapi (fun i p -> (i, p)) (read_from_gsl_caller (n + 1) bin_type parameters)

(*
    return the list of random values with probabilities 
    using GSL C library
*)       
let hyper_to_list n r m =
    let hyper_type = "5" in 
    let parameters = [(string_of_int n); (string_of_int r); (string_of_int m)] in 
    (* read the outputs *)
    List.mapi (fun i p -> (i, p)) (read_from_gsl_caller (m + 1) hyper_type parameters)
