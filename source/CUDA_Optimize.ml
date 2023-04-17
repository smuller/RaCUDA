open CUDA_Types
open Printf
open Types

module C = Cabs

         module IdMap = Map.Make(Id)



(* Levels include Debug:1, Info:2, Warning:3, and Error:4 *)

let log_to_level log_str =
  match log_str with
  | "debug" -> 1
  | "info" -> 2
  | "warn" -> 3
  | "error" -> 4
  | _ -> let () = Format.fprintf Format.std_formatter "WARN - [CUDA_optimize] - Invalid Logging Level! Setting logging level to Debug" in 1

(* Set the logging level here *)
let logging_level = log_to_level("info")

let should_log log = log_to_level(log) >= logging_level 

         
(* While Polynom does a good job with simplification of expressions, this goes a little further *)
let rec expr_eval (expr: 'a cexpr) : 'a cexpr = 
  let cadd (e1, e2) = emk () (CAdd (e1, e2)) in
  let cmul (e1, e2) = emk () (CMul (e1, e2)) in
  let csub (e1, e2) = emk () (CSub (e1, e2)) in
  let cdiv (e1, e2) = emk () (CDiv (e1, e2)) in
  let cmod (e1, e2) = emk () (CMod (e1, e2)) in
  let cconst e = emk () (CConst e) in
  let cl e = emk () (CL e) in
  let rec calculate_binop (expr1: 'a cexpr) (expr2: 'a cexpr) binop binop_float binop_name: 'a cexpr= 
    let e1 = expr_eval expr1 in
    let e2 = expr_eval expr2 in
    match edesc e1, edesc e2 with
    | CConst (CInt v1), CConst (CInt v2) -> cconst(CInt(binop v1 v2))
    | CConst (CFloat f1), CConst (CFloat f2) -> cconst(CFloat(binop_float f1 f2))
    | CConst (CFloat f1), CConst (CInt f2) -> cconst(CFloat(binop_float f1 (float_of_int f2)))
    | CConst (CInt f1), CConst (CFloat f2) -> cconst(CFloat(binop_float (float_of_int f1) f2))
    | CConst v1, CAdd ((annot, CConst v2), e2) | CConst v1, CAdd (e2, (annot, CConst v2)) -> (match v1, v2 with
                      | CFloat f1, CInt v2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CFloat(f1 +. (float_of_int v2))), e2)
                                          | "sub" -> if f1 > float_of_int v2 then cadd(cconst (CFloat(f1 -. float_of_int v2)), e2) else csub(e2, cconst (CFloat(float_of_int v2 -. f1))) 
                                          | "mul" -> cadd(cconst (CFloat(f1 *. float_of_int v2)), cmul(cconst(CFloat f1) ,e2))
                                          | "div" -> cadd(cconst (CFloat(f1 /. float_of_int v2)), cdiv(e2,cconst(CFloat f1)))
                                          | _ -> e2)
                      | CFloat f1, CFloat f2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CFloat(f1 +. f2)), e2)
                                          | "sub" -> if v1 > v2 then cadd(cconst (CFloat(f1 -. f2)), e2) else csub(e2, cconst (CFloat(f2 -. f1))) 
                                          | "mul" -> cadd(cconst (CFloat(f1 *. f2)), cmul(cconst(CFloat f1) ,e2))
                                          | "div" -> cadd(cconst (CFloat(f1 /. f2)), cdiv(e2,cconst(CFloat f1)))
                                          | _ -> e2)
                      | CInt v1, CFloat f2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CFloat((float_of_int v1) +. f2)), e2)
                                          | "sub" -> if (float_of_int v1) > f2 then cadd(cconst (CFloat((float_of_int v1) -. f2)), e2) else csub(e2, cconst (CFloat(f2 -. (float_of_int v1)))) 
                                          | "mul" -> cadd(cconst (CFloat((float_of_int v1) *. f2)), cmul(cconst(CInt v1) ,e2))
                                          | "div" -> cadd(cconst (CFloat((float_of_int v1) /. f2)), cdiv(e2,cconst(CInt v1)))
                                          | _ -> e2)
                      | CInt v1, CInt v2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CInt(v1 + v2)), e2)
                                          | "sub" -> if v1 > v2 then cadd(cconst (CInt(v1 - v2)), e2) else csub(e2, cconst (CInt(v2 - v1))) 
                                          | "mul" -> cadd(cconst (CInt(v1 * v2)), cmul(cconst(CInt v1) ,e2))
                                          | "div" -> cadd(cconst (CInt(v1 / v2)), cdiv(e2,cconst(CInt v1)))
                                          | _ -> e2)
                      | _, _ -> e2
    )
    | CConst v1, CSub ((annot, CConst v2), e2) -> (match v1, v2 with
                      | CFloat f1, CInt v2 -> ( match binop_name with
                                          | "add" -> csub(cconst (CFloat(f1 +. (float_of_int v2))), e2)
                                          | "sub" -> cadd(cconst (CFloat(f1 -. (float_of_int v2))), e2)
                                          | "mul" -> csub(cconst (CFloat(f1 *. float_of_int v2)), cmul(cconst(CFloat f1) ,e2))
                                          | "div" -> csub(cconst (CFloat(f1 /. float_of_int v2)), cdiv(e2,cconst(CFloat f1)))
                                          | _ -> e2)
                      | CFloat f1, CFloat f2 -> ( match binop_name with
                                          | "add" -> csub(cconst (CFloat(f1 +. f2)), e2)
                                          | "sub" -> cadd(cconst (CFloat(f1 -. f2)), e2)
                                          | "mul" -> csub(cconst (CFloat(f1 *. f2)), cmul(cconst(CFloat f1) ,e2))
                                          | "div" -> csub(cconst (CFloat(f1 /. f2)), cdiv(e2,cconst(CFloat f1)))
                                          | _ -> e2)
                      | CInt v1, CFloat f2 -> ( match binop_name with
                                          | "add" -> csub(cconst (CFloat(float_of_int v1 +. f2)), e2)
                                          | "sub" -> cadd(cconst (CFloat(float_of_int v1 -. f2)), e2)
                                          | "mul" -> csub(cconst (CFloat(float_of_int v1 *. f2)), cmul(cconst(CFloat (float_of_int v1)) ,e2))
                                          | "div" -> csub(cconst (CFloat(float_of_int v1 /. f2)), cdiv(e2,cconst(CFloat (float_of_int v1))))
                                          | _ -> e2)
                      | CInt v1, CInt v2 -> ( match binop_name with
                                          | "add" -> csub(cconst (CInt(v1 + v2)), e2)
                                          | "sub" -> cadd(cconst (CInt(v1 - v2)), e2)
                                          | "mul" -> csub(cconst (CInt(v1 * v2)), cmul(cconst(CInt v1) ,e2))
                                          | "div" -> csub(cconst (CInt(v1 / v2)), cdiv(e2,cconst(CInt v1)))
                                          | _ -> e2)
                      | _, _ -> e2
    )
    | CConst v1, CSub (e2, (annot, CConst v2)) -> (match v1, v2 with
                      | CFloat f1, CInt v2 -> ( match binop_name with
                                          | "add" -> if f1 > float_of_int v2 then cadd(e2, (annot, CConst (CFloat(f1-.float_of_int v2)))) else csub(e2, cconst (CFloat(float_of_int v2 -. f1)))
                                          | "sub" -> if f1 > float_of_int v2 then csub(cconst (CFloat(f1-. float_of_int v2)), e2) else cadd(cconst (CFloat(float_of_int v2 -. f1)),e2)
                                          | "mul" -> csub(cmul(cconst(CFloat f1) ,e2), cconst (CFloat(f1 *. float_of_int v2)))
                                          | "div" -> csub(cdiv(e2,cconst(CFloat f1)), cconst (CFloat(f1 /. float_of_int v2)))
                                          | _ -> e2)
                      | CFloat f1, CFloat f2 -> ( match binop_name with
                                          | "add" -> if f1 > f2 then cadd(e2, (annot, CConst (CFloat(f1-.f2)))) else csub(e2, cconst (CFloat(f2 -. f1)))
                                          | "sub" -> if f1 > f2 then csub(cconst (CFloat(f1-. f2)), e2) else cadd(cconst (CFloat(f2 -. f1)),e2)
                                          | "mul" -> csub(cmul(cconst(CFloat f1) ,e2), cconst (CFloat(f1 *. f2)))
                                          | "div" -> csub(cdiv(e2,cconst(CFloat f1)), cconst (CFloat(f1 /. f2)))
                                          | _ -> e2)
                      | CInt v1, CFloat f2 -> ( match binop_name with
                                          | "add" -> if float_of_int v1 > f2 then cadd(e2, (annot, CConst (CFloat(float_of_int v1-.f2)))) else csub(e2, cconst (CFloat(f2 -. float_of_int v1)))
                                          | "sub" -> if float_of_int v1 > f2 then csub(cconst (CFloat(float_of_int v1-. f2)), e2) else cadd(cconst (CFloat(f2 -. float_of_int v1)),e2)
                                          | "mul" -> csub(cmul(cconst(CFloat (float_of_int v1)) ,e2), cconst (CFloat(float_of_int v1 *. f2)))
                                          | "div" -> csub(cdiv(e2, cconst(CFloat (float_of_int v1))), cconst (CFloat(float_of_int v1 /. f2)))
                                          | _ -> e2)
                      | CInt v1, CInt v2 -> ( match binop_name with
                                          | "add" -> if v1 > v2 then cadd(e2, (annot, CConst (CInt(v1-v2)))) else csub(e2, cconst (CInt(v2-v1)))
                                          | "sub" -> if v1 > v2 then csub(cconst (CInt(v1-v2)), e2) else cadd(cconst (CInt(v2-v1)),e2)
                                          | "mul" -> csub(cmul(cconst(CInt v1) ,e2), cconst (CInt(v1 * v2)))
                                          | "div" -> csub(cdiv(e2,cconst(CInt v1)) ,cconst (CInt(v1 / v2)))
                                          | _ -> e2)
                      | _, _ -> e2
    )
    | CConst v1, CMul ((annot, CConst v2), e2) | CConst v1, CMul (e2, (annot, CConst v2)) -> (match v1, v2 with
                      | CFloat f1, CInt v2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CFloat f1), cmul(cconst(CInt v2), e2))
                                          | "sub" -> csub(cconst (CFloat f1), cmul(cconst(CInt v2), e2))
                                          | "mul" -> cmul(cconst (CFloat(f1 *. float_of_int v2)), e2)
                                          | "div" -> cdiv(cconst (CFloat f1), cmul(cconst(CInt v2), e2))
                                          | _ -> e2)
                      | CFloat f1, CFloat f2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CFloat f1), cmul(cconst(CFloat f2), e2))
                                          | "sub" -> csub(cconst (CFloat f1), cmul(cconst(CFloat f2), e2))
                                          | "mul" -> cmul(cconst (CFloat(f1 *. f2)), e2)
                                          | "div" -> cdiv(cconst (CFloat f1), cmul(cconst(CFloat f2), e2))
                                          | _ -> e2)
                      | CInt v1, CFloat f2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CInt v1), cmul(cconst(CFloat f2), e2))
                                          | "sub" -> csub(cconst (CInt v1), cmul(cconst(CFloat f2), e2))
                                          | "mul" -> cmul(cconst (CFloat(float_of_int v1 *. f2)), e2)
                                          | "div" -> cdiv(cconst (CInt v1), cmul(cconst(CFloat f2), e2))
                                          | _ -> e2)
                      | CInt v1, CInt v2 -> ( match binop_name with
                                          | "add" -> cadd(cconst (CInt v1), cmul(cconst(CInt v2), e2))
                                          | "sub" -> csub(cconst (CInt v1), cmul(cconst(CInt v2), e2))
                                          | "mul" -> cmul(cconst (CInt(v1 * v2)), e2)
                                          | "div" -> cdiv(cconst (CInt v1), cmul(cconst(CInt v2), e2))
                                          | _ -> e2)
                      | _, _ -> e2
    )
    | CAdd ((annot, CConst v1), e2), CConst v2 |CAdd (e2, (annot, CConst v1)), CConst v2  -> (match v1, v2 with 
                      | CFloat f1, CInt v2 -> (match binop_name with 
                                          | "add" -> cadd(e2, cconst(CFloat ((float_of_int v2) +. f1)))
                                          | "sub" -> if (float_of_int v2) > f1 then csub(e2, cconst(CFloat ((float_of_int v2) -. f1))) else cadd(e2, cconst(CFloat (f1 -. (float_of_int v2))))
                                          | "mul" -> cadd(cconst(CFloat((float_of_int v2) *. f1)), cmul(cconst(CInt(v2)), e2))
                                          | "div" -> cadd(cconst(CFloat((float_of_int v2) /. f1)), cdiv(cconst(CInt(v2)), e2))
                                          | _ -> e2
                                  )
                      | CFloat f1, CFloat f2 -> (match binop_name with 
                                  | "add" -> cadd(e2, cconst(CFloat (f2 +. f1)))
                                  | "sub" -> if f2 > f1 then csub(e2, cconst(CFloat (f2 -. f1))) else cadd(e2, cconst(CFloat (f1 -. f2)))
                                  | "mul" -> cadd(cconst(CFloat(f2 *. f1)), cmul(cconst(CFloat(f2)), e2))
                                  | "div" -> cadd(cconst(CFloat(f2 /. f1)), cdiv(cconst(CFloat(f2)), e2))
                                  | _ -> e2
                                  )
                      | CInt v1, CFloat f2 -> (match binop_name with 
                                  | "add" -> cadd(e2, cconst(CFloat (f2 +. float_of_int v1)))
                                  | "sub" -> if f2 > float_of_int v1 then csub(e2, cconst(CFloat (f2 -. float_of_int v1))) else cadd(e2, cconst(CFloat (float_of_int v1 -. f2)))
                                  | "mul" -> cadd(cconst(CFloat(f2 *. float_of_int v1)), cmul(cconst(CFloat(f2)), e2))
                                  | "div" -> cadd(cconst(CFloat(f2 /. float_of_int v1)), cdiv(cconst(CFloat(f2)), e2))
                                  | _ -> e2
                                  )           
                      | CInt v1, CInt v2 -> (match binop_name with 
                                  | "add" -> cadd(e2, cconst(CInt (v2 + v1)))
                                  | "sub" -> if v2 > v1 then csub(e2, cconst(CInt (v2 - v1))) else cadd(e2, cconst(CInt (v1 - v2)))
                                  | "mul" -> cadd(cconst(CInt(v2 * v1)), cmul(cconst(CInt(v2)), e2))
                                  | "div" -> cadd(cconst(CInt(v2 / v1)), cdiv(cconst(CInt(v2)), e2))
                                  | _ -> e2
                                  )         
                      | _, _ -> e2
    )
    | CSub ((annot, CConst v1), e2), CConst v2 -> (match v1, v2 with 
                      | CFloat f1, CInt v2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat ((float_of_int v2) +. f1)),e2)
                                  | "sub" -> if (float_of_int v2) > f1 then csub(csub(cconst(CInt(0)),e2), cconst(CFloat ((float_of_int v2) -. f1))) else cadd(csub(cconst(CInt(0)),e2), cconst(CFloat (f1 -. (float_of_int v2))))
                                  | "mul" -> csub(cconst(CFloat((float_of_int v2) *. f1)), cmul(cconst(CInt(v2)), e2))
                                  | "div" -> csub(cconst(CFloat((float_of_int v2) /. f1)), cdiv(cconst(CInt(v2)), e2))
                                  | _ -> e2
                                  )
                      | CFloat f1, CFloat f2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat (f2 +. f1)),e2)
                                  | "sub" -> if f2 > f1 then csub(csub(cconst(CInt(0)),e2), cconst(CFloat (f2 -. f1))) else cadd(csub(cconst(CInt(0)),e2), cconst(CFloat (f1 -. f2)))
                                  | "mul" -> csub(cconst(CFloat(f2 *. f1)), cmul(cconst(CFloat(f2)), e2))
                                  | "div" -> csub(cconst(CFloat(f2 /. f1)), cdiv(cconst(CFloat(f2)), e2))
                                  | _ -> e2
                                  )
                      | CInt v1, CFloat f2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat ((float_of_int v1) +. f2)),e2)
                                  | "sub" -> if f2 > (float_of_int v1) then csub(csub(cconst(CInt(0)),e2), cconst(CFloat (f2 -. float_of_int v1))) else cadd(csub(cconst(CInt(0)),e2), cconst(CFloat (float_of_int v1 -. f2)))
                                  | "mul" -> csub(cconst(CFloat(f2 *. float_of_int v1)), cmul(cconst(CFloat(f2)), e2))
                                  | "div" -> csub(cconst(CFloat(f2 /. float_of_int v1)), cdiv(cconst(CFloat(f2)), e2))
                                  | _ -> e2
                                  )           
                      | CInt v1, CInt v2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CInt (v1 + v2)),e2)
                                  | "sub" -> if v2 > v1 then csub(csub(cconst(CInt(0)),e2), cconst(CInt (v2 - v1))) else cadd(csub(cconst(CInt(0)),e2), cconst(CInt (v1 - v2)))
                                  | "mul" -> csub(cconst(CInt(v2 * v1)), cmul(cconst(CInt(v2)), e2))
                                  | "div" -> csub(cconst(CInt(v2 / v1)), cdiv(cconst(CInt(v2)), e2))
                                  | _ -> e2
                                  )         
                      | _, _ -> e2
    )
    | CSub (e2, (annot, CConst v1)), CConst v2 -> (match v1, v2 with 
                      | CFloat f1, CInt v2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat ((float_of_int v2) +. f1)),e2)
                                  | "sub" -> if (float_of_int v2) > f1 then cadd(e2, cconst(CFloat ((float_of_int v2) -. f1))) else csub(e2, cconst(CFloat ((float_of_int v2) -. f1)))
                                  | "mul" -> csub(cmul(cconst(CInt(v2)), e2), cconst(CFloat((float_of_int v2) *. f1)))
                                  | "div" -> csub(cdiv(cconst(CInt(v2)), e2), cconst(CFloat((float_of_int v2) /. f1)))
                                  | _ -> e2
                                  )
                      | CFloat f1, CFloat f2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat (f2 +. f1)),e2)
                                  | "sub" -> if f2 > f1 then cadd(e2, cconst(CFloat (f2 -. f1))) else csub(e2, cconst(CFloat (f2 -. f1)))
                                  | "mul" -> csub(cmul(cconst(CFloat(f2)), e2), cconst(CFloat(f2 *. f1)))
                                  | "div" -> csub(cdiv(cconst(CFloat(f2)), e2), cconst(CFloat(f2 /. f1)))
                                  | _ -> e2
                                  )
                      | CInt v1, CFloat f2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CFloat ((float_of_int v1) +. f2)),e2)
                                  | "sub" -> if f2 > (float_of_int v1) then cadd(e2, cconst(CFloat (f2 -. float_of_int v1))) else csub(e2, cconst(CFloat (f2 -. float_of_int v1)))
                                  | "mul" -> csub(cmul(cconst(CFloat(f2)), e2), cconst(CFloat(f2 *. float_of_int v1)))
                                  | "div" -> csub( cdiv(cconst(CFloat(f2)), e2), cconst(CFloat(f2 /. float_of_int v1)))
                                  | _ -> e2
                                  )           
                      | CInt v1, CInt v2 -> (match binop_name with 
                                  | "add" -> csub(cconst(CInt (v1 + v2)),e2)
                                  | "sub" -> if v2 > v1 then cadd(e2, cconst(CInt (v2 - v1))) else csub(e2, cconst(CInt (v2 - v1)))
                                  | "mul" -> csub(cmul(cconst(CInt(v2)), e2), cconst(CInt(v2 * v1)))
                                  | "div" -> csub(cdiv(cconst(CInt(v2)), e2), cconst(CInt(v2 / v1)))
                                  | _ -> e2
                                  )         
                      | _, _ -> e2
)
    | CMul (e2, (annot, CConst v1)), CConst v2 |CMul ((annot, CConst v1), e2), CConst v2  -> (match v1, v2 with 
                          | CFloat f1, CInt v2 -> (match binop_name with 
                                      | "add" -> cadd(cmul(e2, cconst(CFloat(f1))), cconst(CInt v2))
                                      | "sub" -> csub(cmul(e2, cconst(CFloat(f1))), cconst(CInt v2))
                                      | "mul" -> cmul(e2, cconst(CFloat(float_of_int v2 *. f1)))
                                      | "div" -> cdiv(cmul(cconst(CFloat(f1)), e2), cconst(CInt(v2)))
                                      | _ -> e2
                                      )
                          | CFloat f1, CFloat f2 -> (match binop_name with 
                                      | "add" -> cadd(cmul(e2, cconst(CFloat(f1))), cconst(CFloat f2))
                                      | "sub" -> csub(cmul(e2, cconst(CFloat(f1))), cconst(CFloat f2))
                                      | "mul" -> cmul(e2, cconst(CFloat(f2 *. f1)))
                                      | "div" -> cdiv(cmul(cconst(CFloat(f1)), e2), cconst(CFloat(f2)))
                                      | _ -> e2
                                      )
                          | CInt v1, CFloat f2 -> (match binop_name with 
                                      | "add" -> cadd(cmul(e2, cconst(CInt(v1))), cconst(CFloat f2))
                                      | "sub" -> csub(cmul(e2, cconst(CInt(v1))), cconst(CFloat f2))
                                      | "mul" -> cmul(e2, cconst(CFloat(f2 *. float_of_int v1)))
                                      | "div" -> cdiv(cmul(cconst(CInt(v1)), e2), cconst(CFloat(f2)))
                                      | _ -> e2
                                      )  
                          | CInt v1, CInt v2 -> (match binop_name with 
                                      | "add" -> cadd(cmul(e2, cconst(CInt(v1))), cconst(CInt v2))
                                      | "sub" -> csub(cmul(e2, cconst(CInt(v1))), cconst(CInt v2))
                                      | "mul" -> cmul(e2, cconst(CInt(v2 * v1)))
                                      | "div" -> cdiv(cmul(cconst(CInt(v1)), e2), cconst(CInt(v2)))
                                      | _ -> e2
                                      )        
                          | _, _ -> e2
    )
                                          

    | (CConst v1, _) -> ( match binop_name with
                      | "add" -> cadd(cconst v1, e2)
                      | "sub" -> csub(cconst v1, e2)
                      | "mul" -> cmul(cconst v1, e2)
                      | "div" -> cdiv(cconst v1, e2)
                      | _ -> e2)
    | (_, CConst v2) -> ( match binop_name with
                        | "add" -> cadd(e1, cconst v2)
                        | "sub" -> csub(e1, cconst v2)
                        | "mul" -> cmul(e1, cconst v2)
                        | "div" -> cdiv(e1, cconst v2)
                        | _ -> e1)
    | _ ->  ( match binop_name with
                        | "add" -> cadd(e1, e2)
                        | "sub" -> csub(e1, e2)
                        | "mul" -> cmul(e1, e2)
                        | "div" -> cdiv(e1, e2)
                        | _ -> e1)
  in
  match edesc expr with
  | CL (CVar id) ->
     (match desc (CUDA.lookup_var id) with
      | EVar x -> cl (CVar x)
      | _ -> expr)
  | CL (CArr (lv, exprs)) -> cl (CArr (lv, List.map expr_eval exprs))
  | CConst _ -> expr
  | CParam _ -> expr
  | CAdd ((annot, CConst(CInt 0)), expr2) -> expr_eval expr2
  | CAdd ((annot, CConst(CFloat 0.0)), expr2) -> expr_eval expr2
  | CAdd (expr1, (annot, CConst(CFloat 0.0))) -> expr_eval expr1
  | CAdd (expr1, (annot, CConst(CInt 0))) -> expr_eval expr1
  | CAdd (expr1, expr2) -> calculate_binop expr1 expr2 (+) (+.) "add"
  | CSub (expr1, (annot, CConst(CFloat 0.0))) -> expr_eval expr1
  | CSub (expr1, (annot, CConst(CInt 0))) -> expr_eval expr1
  | CSub (expr1, expr2) -> calculate_binop expr1 expr2 (-) (-.) "sub"
  | CMul ((annot, CConst(CInt 0)), expr2) -> cconst(CInt 0)
  | CMul ((annot, CConst(CFloat 0.0)), expr2) -> cconst(CInt 0)
  | CMul (expr1, (annot, CConst(CFloat 0.0))) -> cconst(CInt 0)
  | CMul (expr1, (annot, CConst(CInt 0))) -> cconst(CInt 0)
  | CMul ((annot, CConst(CInt 1)), expr2) -> expr_eval expr2
  | CMul ((annot, CConst(CFloat 1.0)), expr2) -> expr_eval expr2
  | CMul (expr1, (annot, CConst(CFloat 1.0))) -> expr_eval expr1
  | CMul (expr1, (annot, CConst(CInt 1))) -> expr_eval expr1
  | CMul (expr1, expr2) -> calculate_binop expr1 expr2  ( * ) ( *. ) "mul"
  | CDiv (expr1, expr2) -> calculate_binop expr1 expr2  ( / ) ( /. ) "div"
  | CMod (expr1, expr2) ->
     let e1 = expr_eval expr1 in
     let e2 = expr_eval expr2 in
     (match edesc e1, edesc e2 with
      | CConst (CInt v1), CConst (CInt v2) ->
        cconst(CInt(v1 mod v2))
      | _ -> cmod(e1, e2))
  | CCall (f, exprs) -> emk () (CCall (f, List.map expr_eval exprs))
  | _ -> expr


(* Evaluate a logical expression *)
let rec logic_eval (logic: 'a clogic) : 'a clogic =
  match logic with
  | CCmp (expr1, cmp, expr2) -> CCmp(expr_eval expr1, cmp, expr_eval expr2)
  | CAnd (logic1, logic2) -> CAnd(logic_eval logic1, logic_eval logic2)
  | COr (logic1, logic2) -> COr(logic_eval logic1, logic_eval logic2)
  | CNot (logic1) -> CNot(logic_eval logic1)

(* Perform constant folding on a code block *)
let rec constant_folding ((annotation, code_list): 'a cblock) : 'a cblock =
  (annotation, fold_constants_list code_list)
and fold_constants_list (code_list: 'a cinstr list) : 'a cinstr list =
  match code_list with
  | [] -> []
  | (code :: rest) ->
     (match code with
      | CDecl _ -> code
      | CAssign(var, exp, b) -> CAssign(var, (expr_eval exp), b)
      | CIf (logic, block1, block2) ->
         CIf(logic_eval logic, constant_folding block1,
             constant_folding block2)
      | CWhile (logic, block1) ->
         CWhile(logic_eval logic, constant_folding block1)
      | CFor (init_block, loop_logic, update_block, loop_body) ->
         CFor (fold_constants_list init_block, logic_eval loop_logic,
               fold_constants_list update_block, constant_folding loop_body)
      | CReturn expr -> CReturn(expr_eval expr)
      | _ -> code)::fold_constants_list rest


let rec are_lists_equal lst1 lst2 eq_function =
  match lst1, lst2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x :: xs, y :: ys -> eq_function x y && are_lists_equal xs ys eq_function

let rec are_consts_equal c1 c2 =
  match c1, c2 with
  | CInt i1, CInt i2 -> i1 = i2
  | CFloat f1, CFloat f2 -> f1 = f2
  | CString s1, CString s2 -> s1 = s2
  | CChar c1, CChar c2 -> c1 = c2
  | CPtr (i1, i2), CPtr (i11, i22) -> i1 = i11 && i2 = i22
  | _, _ -> false

let are_params_equal param1 param2 =
  match param1, param2 with
  | WarpSize, WarpSize -> true
  | GridDim d1, GridDim d2
  | BlockIdx d1, BlockIdx d2
  | BlockDim d1, BlockDim d2
  | ThreadIdx d1, ThreadIdx d2 -> d1 = d2
  | _, _ -> false

let rec are_clvals_equal val1 val2 =
  match val1, val2 with
  | CVar a, CVar b -> a = b
  | CArr (v1, lst1), CArr (v2, lst2) ->
      are_clvals_equal v1 v2 && are_lists_equal lst1 lst2 are_exprs_equal
  | CDeref a, CDeref b -> are_clvals_equal a b
  | CRef a, CRef b -> are_clvals_equal a b
  | _, _ -> false

and are_exprs_equal expr1 expr2 =
  match edesc expr1, edesc expr2 with
  | CL v1, CL v2 -> are_clvals_equal v1 v2
  | CConst c1, CConst c2 -> are_consts_equal c1 c2
  | CParam p1, CParam p2 -> are_params_equal p1 p2
  | CAdd (e1, e2), CAdd (e11, e22)
  | CMul (e1, e2), CMul (e11, e22) ->
      (are_exprs_equal e1 e11 && are_exprs_equal e2 e22)
      || (are_exprs_equal e1 e22 && are_exprs_equal e2 e11)
  | CSub (e1, e2), CSub (e11, e22)
  | CDiv (e1, e2), CDiv (e11, e22)
  | CMod (e1, e2), CMod (e11, e22) -> are_exprs_equal e1 e11 && are_exprs_equal e2 e22
  | CCall (s1, e1), CCall (s2, e2) ->
      s1 = s2 && are_lists_equal e1 e2 are_exprs_equal
  | _, _ -> false

let rec are_logics_equal l1 l2 =
  match l1, l2 with
  | CCmp (e1, c1, e2), CCmp (e11, c2, e22) ->
      c1 = c2
      && ( (c1 = Eq || c1 = Ne)
          && ( (are_exprs_equal e1 e11 && are_exprs_equal e2 e22)
            || (are_exprs_equal e1 e22 && are_exprs_equal e2 e11) )
          || are_exprs_equal e1 e11 && are_exprs_equal e2 e22 )
  | CAnd (cl1, cl2), CAnd (cl11, cl22)
  | COr (cl1, cl2), COr (cl11, cl22) ->
      (are_logics_equal cl1 cl11 && are_logics_equal cl2 cl22)
      || (are_logics_equal cl1 cl22 && are_logics_equal cl2 cl11)
  | CNot c1, CNot c2 -> are_logics_equal c1 c2
  | _, _ -> false

let rec are_instrs_equal instr1 instr2 =
  match instr1, instr2 with
  | CBreak, CBreak | CSync, CSync -> true
  | CAssign (cv1, ce1, b1), CAssign (cv2, ce2, b2) ->
      b1 = b2 && are_clvals_equal cv1 cv2 && are_exprs_equal ce1 ce2
  | CIf (l1, b1, b2), CIf (l11, b11, b22) ->
      are_logics_equal l1 l11 && are_blocks_equal b1 b11 && are_blocks_equal b2 b22
  | CWhile (l1, b1), CWhile (l2, b2) -> are_logics_equal l1 l2 && are_blocks_equal b1 b2
  | CFor (i1, l1, i2, b1), CFor (i11, l11, i22, b11) ->
      are_lists_equal i1 i11 are_instrs_equal && are_logics_equal l1 l11
      && are_lists_equal i2 i22 are_instrs_equal && are_blocks_equal b1 b11
  | CReturn e1, CReturn e2 -> are_exprs_equal e1 e2
  | CDecl (id1, mem1, cabs1, lst1), CDecl (id2, mem2, cabs2, lst2) ->
      id1 = id2 && lst1 = lst2 && mem1 = mem2
  | _, _ -> false

and are_blocks_equal (_, block1) (_, block2) =
  are_lists_equal block1 block2 are_instrs_equal


(** ---------------------------------------------------------------------------------- *)

(* Calculate the complexity score of a single instruction, this can be expanded upon to make it more complex*)
let rec get_complexity_score (line: 'a cinstr) : int =
  match line with
  | CIf (_, block1, block2) ->
     1 + get_complexity_scores block1 + get_complexity_scores block2
  | CWhile (_, block)
  | CFor (_, _, _, block) ->
     let block_score = get_complexity_scores block in
     if block_score > 0 then 3 + block_score else 0 
  | _ -> 1

  (* Calculate the total complexity score of a list of instructions *)
and get_complexity_scores_l (block: 'a cinstr list) : int =
  List.fold_right (fun x y -> get_complexity_score x + y) block 0
and get_complexity_scores ((_, block) : 'a cblock) : int =
  get_complexity_scores_l block

(* Find the longest common code block between two blocks,
   and return the common code and their respective previous and next segments, as well as the max_complexity.
   Complexity cutoff is used to limit the search for common code blocks. We use a dynamic programming approach based on longest_common_substring *)
let longest_common_codeblock ((_, list1): 'a cblock) ((_, list2): 'a cblock) (complexity_cutoff: int) fmt =
  let array1 = Array.of_list list1 and array2 = Array.of_list list2 in
  let length1 = Array.length array1 and length2 = Array.length array2 in
  let () = if should_log "debug" then Format.fprintf fmt "Length of array 1 is %d \n" length1 in
  let () = if should_log "debug" then Format.fprintf fmt "Length of array 2 is %d \n" length2 in
  let counter_matrix = Array.make_matrix (length1 + 1) (length2 + 1) 0 in
  let max_complexity = ref 0 in
  let longest_common_subblock = ref (None, None, None, None, None) in
  for i = 0 to (length1 - 1) do
  for j = 0 to (length2 - 1) do
    if are_instrs_equal array1.(i) array2.(j) then
      let count = counter_matrix.(i).(j) + 1 in
        counter_matrix.(i+1).(j+1) <- count;
      let complexity = get_complexity_scores_l (Array.to_list (Array.sub array1 (i-count+1) count)) in
      if complexity > !max_complexity then
        let () = Format.fprintf fmt "" in
        max_complexity := complexity;
        if !max_complexity >= complexity_cutoff then
          longest_common_subblock := (Some (Array.to_list (Array.sub array1 (i-count+1) count)),
                                      Some (Array.to_list (Array.sub array1 (0) (i-count+1))),
                                      Some (Array.to_list (Array.sub array1 (i+1) (length1-i-1))),
                                      Some (Array.to_list (Array.sub array2 (0) (j-count+1))),
                                      Some (Array.to_list (Array.sub array2 (j+1) (length2-j-1))));
        else longest_common_subblock := !longest_common_subblock;
      done
    done;
  let () = if should_log "debug" then Format.fprintf fmt "Maximum complexity is %d \n" !max_complexity in
  (!longest_common_subblock, !max_complexity)


(* Recursively perform branch distribution for a given block *)
let rec branch_distribution (cutoff: int) ((annotation, code_block): 'a cblock) fmt =
  let () = if should_log "info" then Format.fprintf fmt "\nINFO - [CUDA_optimize] - Running Branch Distribution with cutoff of %d \n" cutoff in
  let rec process_instr_list annotation code_block =
    let process_instr_list = process_instr_list annotation in
    match code_block with
      | [] -> []
      | code :: rest -> 
        (
          match code with 
          | CIf (logic, block1, block2) -> 
            let (common_code, _) = longest_common_codeblock block1 block2 cutoff fmt in
            (
              match common_code with
              | (None, None, None, None, None) ->
                 [CIf (logic, process_annotated_block block1,
                       process_annotated_block block2)]
              | (Some common, Some prev1, Some after1, Some prev2, Some after2) -> (match prev1, after1, prev2, after2 with
               | [], [], [], [] -> process_instr_list common
               | [], _, [], _ -> process_instr_list common @ [CIf(logic, process_annotated_list annotation after1,
                                                                 process_annotated_list annotation after2)]
               | _, [], _, [] -> [CIf(logic, process_annotated_list annotation prev1, process_annotated_list annotation prev2)] @
                                   process_instr_list common
               | _, _, _, _ -> [CIf(logic, process_annotated_list annotation prev1, process_annotated_list annotation prev2)] @
                                 process_instr_list common @
                                   [CIf(logic, process_annotated_list annotation after1, process_annotated_list annotation after2)]
               )
              | _ -> [code]
            )
          | CWhile (logic, block) -> [CWhile(logic, process_annotated_block block)]
          | CFor (lst1, logic, lst2, block) -> [CFor(lst1, logic, lst2, process_annotated_block block)]
          | CAssign (CArr (CVar _, [e]), _, _) -> [code]
          | _ -> [code]
        ) @ process_instr_list rest
  and process_annotated_list annotation lst = (annotation, process_instr_list annotation lst)
  and process_annotated_block (annotation, block) = (annotation, process_instr_list annotation block)
  in process_annotated_block (annotation, code_block)

(* Get the maximum code complexity in a given block *)
let get_max_bd_code_complexity block fmt = 
  let rec helper code_block complexity = 
    match code_block with
    | [] -> complexity
    | code :: rest -> 
      (
        match code with 
          | CIf (logic, (a1, block1), (a2, block2)) -> (
            let (_, score) = longest_common_codeblock (a1, block1) (a2, block2) 0 fmt in
            (* let _ = (if score > 0 then Format.fprintf fmt "GOtta complex of : [%d]\n" score else ()) in *)
            max (max (max (helper block1 complexity) (helper block2 complexity)) (max score complexity)) (helper rest complexity)
          )
          | CWhile (_, (_,block)) -> max (helper block complexity) (helper rest complexity)
          | CFor (_, _, _, (_, block)) -> max (helper block complexity) (helper rest complexity)
          |  _ -> helper rest complexity
        
      )
  in helper block 0 



(* Determine if a parameter has a base type that might contain an array *)
let is_param_bt_array param =
  let (_, bt) = param in
    match bt with
    | C.PTR _ | C.ARRAY _ -> true
    | _ -> false 


(* Processes the bound into a format we can work with *)
let process_bounds id bounds: (string * (CUDA_Types.bound * CUDA_Types.bound * CUDA_Types.bound) option list) = (
  let bound_refs = List.map (!) (List.map ann bounds) in (id, bound_refs)
)

(* Helper function to print out information about the bound *)
let print_is_bound_sharable (lb:CUDA_Types.bound) (ub:CUDA_Types.bound) (size: CUDA_Types.bound) fmt :unit = (
  let _ = Format.fprintf fmt "The lb is %a \n" CUDA.print_cexpr (expr_eval lb) in
  let _ = Format.fprintf fmt "The ub is %a \n" CUDA.print_cexpr (expr_eval ub) in
  let _ = Format.fprintf fmt "The size is %a \n" CUDA.print_cexpr (expr_eval size) in
  let (_,size) = expr_eval size in
  let answer = (match size with
    | CConst(CInt(_)) -> "true"
    | CParam _ -> "true"
    | CL (CVar v) when v = CUDA_Config.bdimx_var
                      || v = CUDA_Config.bdimy_var
                      || v = CUDA_Config.bdimz_var
      ->  "true"
    | _ -> "false")
  in Format.fprintf fmt "Sharable is %s \n\n" answer
)

(* Given the set of lower bound, upper bound and size, determine if a parameter can be moved to shared.fun_args
   NOTE: We might need to keep it conservative, as some bounds can be moved into shared but we can't tell with bounds alone *)
let is_bound_sharable (lb:CUDA_Types.bound) (ub:CUDA_Types.bound) (size: CUDA_Types.bound) fmt :bool = (
  let () = print_is_bound_sharable lb ub size fmt in
  let (_,size) = expr_eval size in
  match size with
  | CConst(CInt(_)) -> true
  | CParam _ -> true
  | CL (CVar v) when v = CUDA_Config.bdimx_var
                     || v = CUDA_Config.bdimy_var
                     || v = CUDA_Config.bdimz_var
    -> true
  | _ -> false
) 

let find_sharable_ids (cleaned_hashtbl:(id, (id * CUDA_Types.bound * CUDA_Types.bound * CUDA_Types.bound * int) list) Hashtbl.t) fmt = (
  let ids = Hashtbl.fold (fun k _ acc -> k :: acc) cleaned_hashtbl [] in
  let is_id_sharable id:bool = (
    let () = Format.fprintf fmt "INFO - [CUDA_optimize] Checking if id %s is sharable\n" id in
    let bounds = Hashtbl.find cleaned_hashtbl id in
    List.for_all (fun x -> x = true) (List.map (fun (_, lb, ub, size, _) -> is_bound_sharable lb ub size fmt) bounds)
  )
  in List.filter is_id_sharable ids
)

(* Find array writes finds all the array writes that are also array type params. It also runs process_bounds, as to get the functional bounds of the arrays*)
let rec find_array_writes block array_param_ids =
  (match block with 
    | [] -> []       
    | cinst :: rest ->
        (match cinst with
          | CIf (_, (_, block1), (_, block2)) -> (find_array_writes block1 array_param_ids) @ (find_array_writes block2 array_param_ids)
          | CWhile (_, (_, block)) -> (find_array_writes block array_param_ids) @ (find_array_writes block array_param_ids)
          | CFor (i1, _, i2, (_, b)) -> (find_array_writes i1 array_param_ids) @ (find_array_writes i2 array_param_ids) @ (find_array_writes b array_param_ids)
          | CAssign(CArr(CVar id, cexprs), _, _) -> if (List.mem id array_param_ids) then [process_bounds id cexprs] else []
          | CAssign(cl, _, _) -> []
          | _ -> []
        ) @ find_array_writes rest array_param_ids
  ) 

let rec find_array_reads block array_param_ids = 
  let rec find_array_reads_clval clv = (match clv with
    | CVar id -> []
    | CArr (CVar(id), cexprs) -> if (List.mem id array_param_ids) then [process_bounds id cexprs] else []
    | CDeref cl -> find_array_reads_clval cl
    | CRef cl -> find_array_reads_clval cl
    | _ -> []
  ) 
  and find_array_reads_cexpr exp = let (_, exp) = exp in (match exp with
    | CL clv -> find_array_reads_clval clv
    | CAdd (ex1, ex2) -> find_array_reads_cexpr ex1  @ find_array_reads_cexpr ex2
    | CSub (ex1, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
    | CMul (ex1, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
    | CDiv (ex1, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
    | CMod (ex1, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
    | CCall (_, cexprs) -> List.flatten (List.map find_array_reads_cexpr cexprs)
    | _ -> []
  )
  and find_array_reads_clogic log = (match log with
    | CCmp (ex1, _, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
    | CAnd (cl1, cl2) -> find_array_reads_clogic cl1 @ find_array_reads_clogic cl2
    | COr (cl1, cl2) -> find_array_reads_clogic cl1 @ find_array_reads_clogic cl2
    | CNot cl1 -> find_array_reads_clogic cl1
  )
in
  (match block with
  | [] -> []
  | cinst :: rest -> (match cinst with
    | CAssign(clv, ex1, _) -> find_array_reads_cexpr ex1 @ find_array_reads rest array_param_ids
    | CIf(cl, (_, cb1), (_, cb2)) -> 
      find_array_reads_clogic cl @ 
      find_array_reads cb1 array_param_ids@ 
      find_array_reads cb2 array_param_ids@ 
      find_array_reads rest array_param_ids
    | CWhile (cl, (_, cb)) -> find_array_reads_clogic cl @ find_array_reads cb array_param_ids @ find_array_reads rest array_param_ids
    | CFor (cinsts, cl, cinsts1, (_, cb)) -> 
      find_array_reads cinsts array_param_ids@ 
      find_array_reads_clogic cl @ 
      find_array_reads cinsts1 array_param_ids@ 
      find_array_reads cb array_param_ids@ 
      find_array_reads rest array_param_ids
    | CReturn ex -> find_array_reads_cexpr ex
    | _ -> find_array_reads rest array_param_ids
  )
) 


(* Finds the bounds of ALL the params, then stores it into the hashtable *)
let gen_bound_hashtbl (t, id, params, block, b) fmt : (id, (id * CUDA_Types.bound * CUDA_Types.bound * CUDA_Types.bound * int) list) Hashtbl.t = (

  let array_params = List.filter is_param_bt_array params in
  let array_param_ids = List.map (fun (id, _) -> id) array_params in 
  let () = if should_log "info" then Format.fprintf fmt "INFO - [CUDA_optimize] - Generating the bounds hashtable\n" in

  (* Now start looking at the blocks *)
  let (_,block_contents) = block in
  let array_writes = find_array_writes block_contents array_param_ids in
  let array_reads = find_array_reads block_contents array_param_ids in
  (* Generating the hashtable 
     bounds_hashtbl is an intermediate hashtable that stores the bounds of all the params, which will eventually get cleaned up into cleaned_hashtbl
     cleaned_hashtbl is the final hashtable that stores the bounds of all the params, with duplicates removed. 

     key: id of the param
    value: list of (id, lb, ub, diff, variant number]) tuples
  *)

  let bounds_hashtbl = Hashtbl.create 50 in
  let cleaned_hashtbl = Hashtbl.create 50 in
  let populate_hashtbl bounds_list = 
    let add_to_hashtbl id bound_ref =
      match bound_ref with
        | None -> Hashtbl.add bounds_hashtbl id None
        | Some (lb, ub, diff) -> Hashtbl.add bounds_hashtbl id (Some (id, lb, ub, diff)) in

    let process_single_bound (id, bounds_refs) =  let _ = (List.map (add_to_hashtbl id) bounds_refs) in () in
    let _ = (List.map process_single_bound bounds_list) in () in

  let () = populate_hashtbl array_reads in
  let () = populate_hashtbl array_writes in

  (* Clean hashtbl removes the duplicates  *)
  let clean_hashtbl = 
    let is_some opt =
      match opt with
      | None -> false
      | Some _ -> true in
      
    let to_result opt = 
      match opt with 
      | None -> ("a", emk () (CConst(CInt 0)), emk () (CConst(CInt 0)), emk () (CConst(CInt 0)))
      | Some (id, lb, ub, diff) -> (id, lb, ub, diff) in

    let clean_single_id id = 
      let values = Hashtbl.find_all bounds_hashtbl id in
      let () = if List.mem None values then if should_log "warn" then Format.fprintf fmt "%s is missing bounds \n" id else () in
      let non_none_values = List.map to_result (List.filter is_some values) in

      let rec helper (vals) (current) = 
        match vals with
        | [] -> current
        | value::rest -> if List.mem value current then helper rest current else helper rest (current @ [value]) in
      
      let cleaned = helper non_none_values [] in 

      let rec add_variant_num values variant_num = 
        match values with
        | [] -> []
        | (id, lb, ub, diff)::rest -> [(id, lb, ub, diff, variant_num)] @ add_variant_num rest (variant_num + 1) in

      let cleaned_with_variant_num = add_variant_num cleaned 1 in
      Hashtbl.add cleaned_hashtbl id cleaned_with_variant_num in

    let _ = (List.map clean_single_id array_param_ids) in () in

  let () = clean_hashtbl in cleaned_hashtbl
)

(* This takes in a func, the list parameters we want to move to shared, and the cleaned_hashtbl of bounds as well as a formatter 
    and returns a new func with the bounds of the parameters added to the shared memory
   *)
let new_global_to_shared_opt ((t, id, params, block, b): 'a cfunc) used_params cleaned_hashtbl fmt : unit cfunc = (

    (* Work with getting the right array params *)
    
    let is_in_used param = List.mem param used_params in

    let array_params = List.filter is_param_bt_array params in
    let used_array_params = List.filter (is_in_used) array_params in

    let array_param_ids = List.map (fun (id, _) -> id) used_array_params in 
    let (_,block_contents) = block in
    let array_writes = find_array_writes block_contents array_param_ids in
    let array_writes_ids = List.map (fun (id, _) -> id) array_writes in
    let () = if should_log "info" then Format.fprintf fmt "INFO - [CUDA_optimize] - Running Global to Shared on array params: [%s]\n" (String.concat "," array_param_ids) in

    (* Helper code for code generation portion *)
    (* Defines the variable that stores (blockIdx.x * blockDim.x) + threadIdx.x *)
    let thread_position_var = emk () (CAdd((emk () (CMul(emk () (CL(CVar("blockIdx.x"))), emk () (CL(CVar("blockDim.x")))))), emk () (CL(CVar("threadIdx.x"))))) in
    let get_pointer_bt t = 
      match t with
      | C.ARRAY(bt, expr) -> bt
      | C.PTR bt -> bt
      | _ -> t in

    (* Converts into a clval type *)
    let rec param_to_clval (id, t, arr_idx) ind =
      match arr_idx with
      | Some arr_idx -> 
        (match (t, ind) with
        | (C.PTR b_t, _) -> CArr(param_to_clval (id, b_t, Some (arr_idx)) ind, [arr_idx])
        | (C.ARRAY (b_t, exp), Some i) -> CArr(param_to_clval (id, b_t, Some(arr_idx)) ind, [i])
        | _ -> CVar id
            )
      | None -> 
        (match (t, ind) with
        | (C.PTR b_t, _) -> CRef (param_to_clval (id, b_t, None) ind)
        | (C.ARRAY (b_t, exp), Some i) -> CArr(param_to_clval (id, b_t, None) ind, [i])
        | _ -> CVar id
          ) in  
    
    (* Define the shared arrays and copy globals into them *)
    let global_convert_to_shared = 
      let bring_bound_to_shared (id, bt) (_, lb, ub, diff, variant_num) = (
        let pointer_bt = get_pointer_bt bt in
        let variant_num_str = string_of_int variant_num in
        let tvar_as_exp = emk () (CL (CVar "__itertemp")) in
        [
          (* CDecl ("size_" ^ id ^ variant_num_str, Local, C.INT(C.LONG, C.SIGNED), []);
          CDecl ("lower_bound_" ^ id ^ variant_num_str, Local, C.INT(C.LONG, C.SIGNED), []); *)
          (* CDecl ("upper_bound_" ^ id ^ variant_num_str, Local, C.INT(C.LONG, C.SIGNED), []); *)
          (* CAssign(CVar("upper_bound_" ^ id ^ variant_num_str), expr_eval ub, true); *)
          (* CAssign(CVar("lower_bound_" ^ id ^ variant_num_str), expr_eval lb, true);
          CAssign(CVar("size_" ^ id ^ variant_num_str), expr_eval diff, true); *)
          CDecl(id ^ variant_num_str, Shared, C.ARRAY(pointer_bt, C.VARIABLE ("BLOCKSIZE")), []);
          (CAssign ((param_to_clval (id ^ variant_num_str,
                                     bt,
                                     Some (emk () (CL(CVar("threadIdx.x")))))
                                     (Some tvar_as_exp)
                    ),
                    (emk () (CL (param_to_clval (id, bt, Some  (thread_position_var)) (Some tvar_as_exp)))) ,true))
        ]) in 

      let param_id_to_shared id = 
        let param_pair = List.find (fun (x, _) -> x = id) array_params in
        let bounds_list = (Hashtbl.find cleaned_hashtbl id) in
        List.flatten(List.map (bring_bound_to_shared param_pair) bounds_list) in
      
      List.flatten(List.map param_id_to_shared array_param_ids) @ [CSync] in
    
    (* Copy the changes from the shared arrays back into the globals. It only runs performs the operations on parameters that have been written to *)
    let shared_back_to_global = 
      let rec intersection lst1 lst2 =
        match lst1 with
        | [] -> []
        | hd::tl ->
            if List.mem hd lst2 then
              hd :: intersection tl lst2
            else
              intersection tl lst2 in

      let shared_back_to_global_helper (id, bt) (_, lb, ub, diff, variant_num)=
        let tvar_as_exp = emk () (CL (CVar "__itertemp")) in
        let variant_num_str = string_of_int variant_num in                            
          (CAssign ((param_to_clval (id,
                                     bt,
                                     Some (thread_position_var))
                                     (Some tvar_as_exp)
                    ),
                    (emk () (CL (param_to_clval (id ^ variant_num_str, bt, Some (emk () (CL(CVar("threadIdx.x"))))) (Some tvar_as_exp)))) ,true)) in

        let param_id_to_global id = 
          let param_pair = List.find (fun (x, _) -> x = id) params in
          let bounds_list = (Hashtbl.find cleaned_hashtbl id) in
          List.map (shared_back_to_global_helper param_pair) bounds_list in

      List.flatten(List.map param_id_to_global (intersection array_param_ids array_writes_ids)) in
      
      (* Useless bound is just there to satisfy the type requirements, and it will eventually get replaced with a unit. *)
      let useless_bound = ref (Some (emk () (CConst(CInt(0))), (emk () (CConst(CInt(0)))), emk () (CConst(CInt(0))))) in

      let is_bound_valid bound_refs:bool =
        let bound_ref = List.hd bound_refs in
        match bound_ref with
        | None -> false
        | Some (lb, ub, _) -> true in

      let bound_to_variant_num id bound_refs:int =
        let is_bounds_same lb ub (hashtbl_entry:(Types.id * unit CUDA_Types.cexpr * unit CUDA_Types.cexpr * unit CUDA_Types.cexpr * int)):bool = 
          let (_, lb_h, ub_h, _, _) = hashtbl_entry in List.mem lb [lb_h] && List.mem ub [ub_h] in

        let bound_ref = List.hd bound_refs in
        match bound_ref with
        | None -> 0
        | Some (lb, ub, _) -> let id_bounds = Hashtbl.find cleaned_hashtbl id in
                           let (_, _, _, _, variant_num) = List.find (is_bounds_same lb ub) id_bounds in
                           variant_num in

      (* Renaming means replacing the array variables with their respective shared memory variables *)
      let rec rename_clval (c: 'a clval): 'a clval = 
        
        (match c with
        | CArr(CVar id, fst_exp :: rest_exp_lst) -> 
          (if List.mem id array_param_ids
            then
              let (_ ,this_bound) = process_bounds id (fst_exp :: rest_exp_lst) in
              if is_bound_valid this_bound then 
                let variant_num_str = string_of_int (bound_to_variant_num id this_bound) in
                CArr(CVar (id ^ variant_num_str),
                  emk useless_bound (CL(CVar("threadIdx.x"))) :: (List.map rename_cexpr rest_exp_lst))
              else
                CArr(CVar id, (List.map rename_cexpr (fst_exp :: rest_exp_lst)))
            else CArr(CVar id, (List.map rename_cexpr (fst_exp :: rest_exp_lst))))
        | CVar id -> CVar id
        | CArr (cv, exp_lst) -> CArr(rename_clval cv, (List.map rename_cexpr exp_lst))
        | CDeref clv -> CDeref(rename_clval clv)
        | CRef clv -> CRef(rename_clval clv)
        )
         and rename_cexpr (exp: 'a cexpr): 'a cexpr =
           emk useless_bound
        (match edesc exp with 
        | CL clv -> CL(rename_clval clv)
        | CAdd(exp1, exp2) -> CAdd(rename_cexpr exp1, rename_cexpr exp2)
        | CSub(exp1, exp2) -> CSub(rename_cexpr exp1, rename_cexpr exp2)
        | CMul(exp1, exp2) -> CMul(rename_cexpr exp1, rename_cexpr exp2)
        | CDiv(exp1, exp2) -> CDiv(rename_cexpr exp1, rename_cexpr exp2)
        | CMod(exp1, exp2) -> CMod(rename_cexpr exp1, rename_cexpr exp2)
        | CCall(st, exps) -> CCall(st, List.map rename_cexpr exps)
        | ed -> ed
        )
    
      and rename_clogic (cl: 'a clogic): 'a clogic = 
        (match cl with
        | CCmp(exp1, cmp, exp2) -> CCmp(rename_cexpr exp1, cmp, rename_cexpr exp2)
        | CAnd(cl1, cl2) -> CAnd(rename_clogic cl1, rename_clogic cl2)
        | COr(cl1, cl2) -> COr(rename_clogic cl1, rename_clogic cl2)
        | CNot cl1 -> CNot(rename_clogic cl1)
        )
      and rename_cinstr (code_instr: Graph_Types.annot CUDA_Types.cinstr): 'a cinstr =  
        match code_instr with
        | CAssign(cl, ce, b) -> CAssign(rename_clval cl, rename_cexpr ce, b)
        | CIf(cl, cb1, cb2) ->
           CIf(rename_clogic cl, rename_cblock cb1,
               rename_cblock cb2)
        | CWhile(cl, cb) -> CWhile(rename_clogic cl, rename_cblock cb)
        | CFor(cb1, cl, cb2, cb3) ->
           CFor(List.map rename_cinstr cb1, rename_clogic cl,
                List.map rename_cinstr cb2, rename_cblock cb3)
        | CReturn(ce) -> CReturn(rename_cexpr ce)
        | _ -> code_instr

      and rename_cblock (_, block) = (useless_bound, List.map rename_cinstr block) in
        let (a, bl) = rename_cblock block in
        let bl_erased = List.map erase_instr bl in 
        let arrs =
         (* let _ = print_string (String.concat ", " (List.map (fun ((id, _, _) , _) -> id) (get_param_bounds params))) in *)
         (t, id, params, ((), 
                          global_convert_to_shared
                         @ bl_erased
                         @ shared_back_to_global), b)
         in arrs
    )

 let branch_distribution_prog (cutoff: int) ((globals, funcs): 'a cprog) (used_args) bounds_hashtbl fmt =

  let bdf (rt, name, args, code, kernel) =
    let distributed = 
      if cutoff > ~-1 
        then branch_distribution cutoff code fmt
        else (let _ = if should_log "info" then Format.fprintf fmt "\nINFO - [CUDA_optimize] - Not running branch distribution \n" in code) in 
    new_global_to_shared_opt (rt, name, args, distributed, kernel) used_args bounds_hashtbl fmt
  in
  (globals, List.map bdf funcs)
   
(* Generates a list of (branch_distribution_cutoff, code) for the OptDriver to find the best one of. *)
let greedy_find_branch_distribution_cutoffs (prog: Graph_Types.annot cprog) fmt = 
  let (globals, funcs) = prog in

  (* ASSUMPTION THAT THERE IS ONLY ONE FUNCTION IN A FILE! (or that the arguments in each function are the same) *)

  let (_, _, params, (_, code), _) = List.hd funcs in
  let complexity_score = get_max_bd_code_complexity code fmt in

  let half = complexity_score / 2 in
  let half_point = if complexity_score mod 2 = 0 then half else half + (if complexity_score < 0 then -1 else 1) in

  (* Bounds hashtable is empty because it does not run global_to_shared *)
  let empty_hashtbl: (id, (id * (unit * unit CUDA_Types.cexpr_) * (unit * unit CUDA_Types.cexpr_) * (unit * unit CUDA_Types.cexpr_) * int) list) Hashtbl.t = Hashtbl.create 10 in

  let () = Format.fprintf fmt "\n MAX COMPLEXITY IS %d \n" complexity_score in
  let bdp c used_array_params = branch_distribution_prog c prog used_array_params empty_hashtbl fmt in
  let bdp_params = 
    if complexity_score > 10 then
      [(0, [], bdp 0 []); 
      (half_point, [], bdp half_point []); 
      (complexity_score-1, [], bdp (complexity_score-1) [])]
    else if complexity_score > 0 then
      [(0, [], bdp 0 [])] 
    else [] in
    bdp_params

let greedy_find_array_params (prog: 'a cprog) cutoff current_params sharable_params  cleaned_hashtbl fmt = 
  let (globals, funcs) = prog in
  let (_, _, params, (_, code), _) = List.hd funcs in
  let array_params = List.filter is_param_bt_array params in
  
  (* Params that have not already been used *)
  let potential_params = List.filter (fun x -> (not (List.mem x current_params) && (let (id, _) = x in List.mem id sharable_params ))) array_params in

  let bdp_potential potential_param = (cutoff, (current_params @ [potential_param]), (branch_distribution_prog cutoff prog (current_params @ [potential_param]) cleaned_hashtbl fmt)) in
  List.map bdp_potential potential_params

  
  
