open CUDA_Types
open Printf
open Types

module C = Cabs

         module IdMap = Map.Make(Id)

                      (*
let 
_to_CUDA (lbs, ubs) =
  let bd_to_CUDA neg (k, l) =
    let k = if neg then ~-k else k in
    CDiv (Presburger.L.toCUDA l, CConst (CInt k))
  in
  let _ = Printf.printf "%d lbs, %d ubs\n" (List.length lbs) (List.length ubs)
  in
  match (lbs, ubs) with
  | ([lb], [ub]) -> Some (bd_to_CUDA false lb, bd_to_CUDA true ub)
  | _ -> None
                       *)
         
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


let rec logic_eval (logic: 'a clogic) : 'a clogic = 
  match logic with
  | CCmp (expr1, cmp , expr2) -> CCmp(expr_eval expr1, cmp , expr_eval expr2)
  | CAnd (logic1, logic2) -> CAnd(logic_eval logic1, logic_eval logic2)
  | COr (logic1, logic2) -> COr(logic_eval logic1, logic_eval logic2)
  | CNot (logic1) -> CNot(logic_eval logic1)


let rec constant_folding ((a, code): 'a cblock) : 'a cblock = 
  (a, cf_list code)
and cf_list (l: 'a cinstr list) : 'a cinstr list =
  match l with
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
      | CFor (block1, logic, block2, block3) ->
         CFor (cf_list block1, logic_eval logic,
               cf_list block2, constant_folding block3)
      | CReturn expr -> CReturn(expr_eval expr)
      | _ -> code)::cf_list rest


let rec is_lst_equal(lst1) (lst2) equality_function: bool = 
  match lst1, lst2 with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | x::xs, y::ys -> equality_function x y && is_lst_equal xs ys equality_function

let rec is_const_equal(c1: const) (c2: const) : bool =
  match c1, c2 with
  | CInt i1, CInt i2 -> i1 = i2
  | CFloat f1, CFloat f2 -> f1 = f2
  | CString s1, CString s2 -> s1 = s2
  | CChar c1, CChar c2 -> c1 = c2
  | CPtr (i1, i2), CPtr(i11, i22) -> (i1 = i11) && (i2 = i22)
  | _ , _ -> false

let is_param_equal(param1: cparam) (param2: cparam) : bool = 
  let is_dim_equal (d1: dim) (d2: dim) : bool = 
    match d1, d2 with
    | X, X -> true
    | Y, Y -> true
    | Z, Z -> true
    | _ , _ -> false

  in
  match param1, param2 with
  | WarpSize, WarpSize -> true
  | GridDim d1, GridDim d2 -> is_dim_equal d1 d2
  | BlockIdx b1, BlockIdx b2 -> is_dim_equal b1 b2
  | BlockDim b1, BlockDim b2 -> is_dim_equal b1 b2
  | ThreadIdx t1, ThreadIdx t2 -> is_dim_equal t1 t2
  | _ , _ -> false

let rec is_clval_equal (val1 : 'a clval) (val2 : 'a clval) : bool = 
  match val1, val2 with
  | CVar a, CVar b -> a = b
  | CArr (v1, lst1), CArr(v2, lst2) -> is_clval_equal v1 v2 && is_lst_equal lst1 lst2 is_expr_equal
  | CDeref a, CDeref b -> is_clval_equal a b
  | CRef a, CRef b -> is_clval_equal a b
  | _ , _ -> false 
  
and is_expr_equal(expr1: 'a cexpr) (expr2: 'a cexpr) : bool = 
  match edesc expr1, edesc expr2 with
  | CL v1, CL v2 -> is_clval_equal v1 v2
  | CConst c1, CConst c2 -> is_const_equal c1 c2
  | CParam p1, CParam p2 -> is_param_equal p1 p2
  | CAdd (e1, e2), CAdd(e11, e22) -> (is_expr_equal e1 e11 && is_expr_equal e2 e22) || (is_expr_equal e1 e22 && is_expr_equal e2 e11)
  | CSub (e1, e2), CSub(e11, e22) -> is_expr_equal e1 e11 && is_expr_equal e2 e22
  | CMul (e1, e2), CMul(e11, e22) -> (is_expr_equal e1 e11 && is_expr_equal e2 e22) || (is_expr_equal e1 e22 && is_expr_equal e2 e11)
  | CDiv (e1, e2), CDiv(e11, e22) -> is_expr_equal e1 e11 && is_expr_equal e2 e22
  | CMod (e1, e2), CMod(e11, e22) -> is_expr_equal e1 e11 && is_expr_equal e2 e22
  | CCall (s1, e1), CCall(s2, e2) -> if s1 = s2 then (is_lst_equal e1 e2 is_expr_equal) else false
  | _ , _ -> false  

let rec is_logic_equal(l1: 'a clogic) (l2: 'a clogic) : bool =
    match l1, l2 with
    | CCmp (e1, c1, e2), CCmp(e11, c2, e22) -> 
      if c1 = c2 then (
        match c1 with 
        | Eq -> (is_expr_equal e1 e11 && is_expr_equal e2 e22) || (is_expr_equal e1 e22 && is_expr_equal e2 e11)
        | Ne -> (is_expr_equal e1 e11 && is_expr_equal e2 e22) || (is_expr_equal e1 e22 && is_expr_equal e2 e11)
        | _ -> is_expr_equal e1 e11 && is_expr_equal e2 e22
        ) else false
    | CAnd (cl1, cl2), CAnd (cl11, cl22) -> ((is_logic_equal cl1 cl11) && (is_logic_equal cl2 cl22 )) || ((is_logic_equal cl1 cl22) && (is_logic_equal cl2 cl11))
    | COr (cl1, cl2), COr(cl11, cl22) -> ((is_logic_equal cl1 cl11) && (is_logic_equal cl2 cl22 )) || ((is_logic_equal cl1 cl22) && (is_logic_equal cl2 cl11))
    | CNot c1, CNot c2 -> is_logic_equal c1 c2
    | _, _ -> false
  
  
let rec is_instr_equal(instr1: 'a cinstr) (instr2: 'a cinstr) : bool = 
  match instr1, instr2 with
  | CBreak, CBreak -> true
  | CAssign (cv1, ce1, b1), CAssign(cv2, ce2, b2) -> if b1 = b2 then (is_clval_equal cv1 cv2) && (is_expr_equal ce1 ce2) else false  
  | CIf (l1, b1, b2), CIf(l11, b11, b22) -> is_logic_equal l1 l11 && is_block_equal b1 b11 && is_block_equal b2 b22
  | CWhile (l1, b1), CWhile(l2, b2) -> is_logic_equal l1 l2 && is_block_equal b1 b2
  | CFor(i1, l1, i2, b1), CFor(i11, l11, i22, b11) -> (is_lst_equal i1 i11 is_instr_equal) && (is_logic_equal l1 l11) && (is_lst_equal i2 i22 is_instr_equal) && is_block_equal b1 b11
  | CReturn e1, CReturn e2 -> is_expr_equal e1 e2
  | CSync, CSync -> true
  | CDecl (id1, mem1, cabs1, lst1), CDecl (id2, mem2, cabs2, lst2) -> id1 = id2 && lst1 = lst2 && (mem1 = mem2)
  | _ , _ -> false


and is_block_equal((_, block1): 'a cblock) ((_, block2): 'a cblock) : bool = 
  is_lst_equal block1 block2 is_instr_equal


(** ---------------------------------------------------------------------------------- *)


let rec get_complexity_score (line: 'a cinstr) : int =
  match line with
  | CIf (_, block1, block2) ->
     1 + get_complexity_scores block1 + get_complexity_scores block2
  | CWhile (_, block) ->
     let block_score = get_complexity_scores block in
     if block_score > 0 then 3 + block_score else 0 
  | CFor (_, _, _, block) ->
     let block_score = get_complexity_scores block in
     if block_score > 0 then 3 + block_score else 0 
  | _ -> 1
and get_complexity_scores_l (block: 'a cinstr list) : int =
  List.fold_right (fun x y -> get_complexity_score x + y) block 0
and get_complexity_scores ((_, block) : 'a cblock) : int =
  get_complexity_scores_l block

(** Return type is (Common, lst1 prev, lst1 after, lst2 prev, lst2 after) *)
let longest_common_codeblock ((_, lst1): 'a cblock) ((_, lst2): 'a cblock)
      (complexity_cutoff: int) =
  let arr1 = Array.of_list lst1 and arr2 = Array.of_list lst2 in
  let m = Array.length arr1 and n = Array.length arr2 in
  let () = Format.fprintf Format.std_formatter "Length of array 1 is %d \n" m in
  let () = Format.fprintf Format.std_formatter "Length of array 2 is %d \n" n in
  let counter = Array.make_matrix (m+1) (n+1) 0 in
  let max_complexity = ref 0 in
  let lcs = ref (None, None, None, None, None) in
  for i = 0 to (m - 1) do
    for j = 0 to (n - 1) do
      if is_instr_equal arr1.(i) arr2.(j) then
        let c = counter.(i).(j) + 1 in
          counter.(i+1).(j+1) <- c;
        let complexity = get_complexity_scores_l (Array.to_list (Array.sub arr1 (i-c+1) c)) in
        (* let () = Format.fprintf Format.std_formatter "C = %d, I = %d, J = %d\n" c i j in  *)
        if complexity > !max_complexity then
          let () = Format.fprintf Format.std_formatter "" in
          max_complexity := complexity;
          if !max_complexity >= complexity_cutoff then
          lcs := (Some (Array.to_list (Array.sub arr1 (i-c+1) c)),
                  Some (Array.to_list (Array.sub arr1 (0) (i-c+1))),
                  Some (Array.to_list (Array.sub arr1 (i+1) (m-i-1))),
                  Some (Array.to_list (Array.sub arr2 (0) (j-c+1))),
                  Some (Array.to_list (Array.sub arr2 (j+1) (n-j-1))));
        else lcs := !lcs;
        done
      done;
  let () = Format.fprintf Format.std_formatter "Maximum complexity is %d \n" !max_complexity in
 !lcs

(* This is really inefficient, we should fix it *)
let rec branch_distribution (c: int) ((a, code_block): 'a cblock) =
  let rec bd_ll a code_block =
    let bd_ll = bd_ll a in
    match code_block with
 | [] -> []
 | code :: rest -> 
   (
     match code with 
     | CIf (logic, block1, block2) -> 
       (
         match longest_common_codeblock block1 block2 c with
         | (None, None, None, None, None) ->
            [CIf (logic, bd_bb block1,
                  bd_bb block2)]
         | (Some common, Some prev1, Some after1, Some prev2, Some after2) -> (match prev1, after1, prev2, after2 with
          | [], [], [], [] -> bd_ll common
          | [], _, [], _ -> bd_ll common @ [CIf(logic, bd_lb a after1,
                                                bd_lb a after2)]
          | _, [], _, [] -> [CIf(logic, bd_lb a prev1, bd_lb a prev2)] @
                              bd_ll common
          | _, _, _, _ -> [CIf(logic, bd_lb a prev1, bd_lb a prev2)] @
                            bd_ll common @
                              [CIf(logic, bd_lb a after1, bd_lb a after2)]
          )
         | _ -> [code]
       )
     | CWhile (logic, block) -> [CWhile(logic, bd_bb block)]
     | CFor (lst1, logic, lst2, block) -> [CFor(lst1, logic, lst2, bd_bb block)]
     | CAssign (CArr (CVar _, [e]), _, _) ->
        (*(match !(ann e) with
         | None -> Printf.printf "not annotated"
         | Some (lb, ub) ->
            Format.fprintf Format.std_formatter "%a in [%a, %a]\n"
              CUDA.print_cexpr e
              CUDA.print_cexpr (expr_eval lb)
              CUDA.print_cexpr (expr_eval ub)
        );*) [code]
     | _ -> [code]
   ) @ bd_ll rest
  and bd_lb a l = (a, bd_ll a l)
  and bd_bb (a, b) = (a, bd_ll a b)
  and bd_bl (a, b) = bd_ll a b
  in bd_bb (a, code_block)

type bound = unit expr
type ablock = (bound * bound) option ref cblock


let rec find_array_ids vctx p m ((annot, block): ablock)  = 
  let rec find_clval_arr_id (clv: 'a clval): (string * bound * bound) list = 
    match clv with
    | CArr( CVar(clv), [cexpr]) ->
       (match !(ann cexpr) with
        | None -> failwith "not annotated"
        | Some (lb, ub) -> [(clv, lb, ub)]
       )
    | CDeref clv -> find_clval_arr_id clv
    | CRef clv -> find_clval_arr_id clv
    | _ -> []
  

  in
  match block with
  | [] -> []
  | code :: rest ->
    (match code with
    | CAssign(clv, ce, b) -> find_clval_arr_id clv
    | _ -> []
    ) @ find_array_ids vctx p m (annot, rest)


  (* This is to determine the original base type of the pointer, so we can convert it to an array *)
  let get_pointer_bt t = 
    match t with
    | C.ARRAY(bt, expr) -> bt
    | C.PTR bt -> bt
    | _ -> t

    let uniq_cons x xs = let (x_id, _, _) = x in if List.mem x_id (List.map (fun (x_id, _, _) -> x_id) xs) then xs else x :: xs
    (* This is what I used to use instead of id_lbs_and_ubs back when there was only one upper and lower bound *)
    let remove_from_right xs = List.fold_right uniq_cons xs []
    
    (* This is basically a key-value pair of the ids and their upper and lower bounds. Right now, it doesn't work well if there is a list of lbs or ubs in the input, but haven't come across it yet*)
    let id_lbs_and_ubs xs = 
      let helper dict (id, lb, ub) =  
        let ids = List.map (fun(id, _, _) -> id) dict in
        if List.mem id ids then 
          let (id, current_lbs, current_ubs) = List.find (fun (x, _, _) -> x = id) dict in
          let removed = List.filter (fun (x, _, _) -> not (x = id)) dict in 
          (if List.mem (List.hd lb) current_lbs then 
            (if List.mem (List.hd ub) current_ubs then
              dict
            else 
              removed @ [(id, current_lbs, current_ubs @ ub)])
          else
            (if List.mem (List.hd ub) current_ubs then
              removed @ [(id, current_lbs @ lb, current_ubs)]
            else 
              removed @ [(id, current_lbs @ lb, current_ubs @ ub)]))  
        else dict @ [(id, lb, ub)]
      in let ret = List.fold_left helper [] xs 
    
    (* in let print_debug (id, lbs, ubs) =
        Format.fprintf Format.std_formatter "ID of %s in with LBS %a AND UBS %a\n"
        id
        CUDA.print_cexpr_list (List.map expr_eval lbs)
        CUDA.print_cexpr_list (List.map expr_eval ubs) in
    
      let useless = List.map print_debug ret  *)
    
    
      in ret
    
    let global_to_shared (t, id, params, block, b): 'a cfunc =
    
    
      (* We used to only take the first element in the bounds array, now we process all them *)
      let process_bounds id bounds = (
        let bounds_helper (lb_lst, ub_lst) b = 
          match b with
          | None -> (lb_lst, ub_lst)
          | Some (lb, ub) -> (lb_lst @ [lb], ub_lst @ [ub])
        in
    
    
        let bound_refs = List.map (!) (List.map ann bounds) in
        let upper_and_lower_bounds = List.fold_left bounds_helper ([], []) bound_refs in
        match upper_and_lower_bounds with 
        | ([], []) -> []
        | (lb, ub) -> [(id, lb, ub)]
      ) in
    
      let rec find_array_writes block =
        (match block with 
         | [] -> []
         
        | cinst :: rest ->
           (match cinst with
            | CIf (_, (_, block1), (_, block2)) ->
               (find_array_writes block1) @ (find_array_writes block2)
            | CWhile (_, (_, block)) ->
               (find_array_writes block) @ (find_array_writes block)
            | CFor (i1, _, i2, (_, b)) ->
               (find_array_writes i1) @ (find_array_writes i2)
               @ (find_array_writes b)
            | CAssign(CArr(CVar id, cexprs), _, _) -> (process_bounds id cexprs)
            | CAssign(cl, _, _) ->
               (* Format.fprintf Format.std_formatter "Writing to: ";
               CUDA.print_clval Format.std_formatter cl;
               Format.print_newline (); *)
               []
          | _ -> []
           ) @ find_array_writes rest
        )
      in let rec find_array_reads block = 
    
        let rec find_array_reads_clval clv = (match clv with
          | CVar id -> []
          | CArr (CVar(id), cexprs) ->(process_bounds id cexprs) @ List.flatten (List.map find_array_reads_cexpr cexprs)
          | CDeref cl -> find_array_reads_clval cl
          | CRef cl -> find_array_reads_clval cl
          | _ -> []
        ) 
        and find_array_reads_cexpr exp = let (_, exp) = exp in (match exp with
          | CL clv -> find_array_reads_clval clv
          | CAdd (ex1, ex2) -> find_array_reads_cexpr ex1 @ find_array_reads_cexpr ex2
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
          | CAssign(clv, ex1, _) -> find_array_reads_cexpr ex1 @ find_array_reads rest
          | CIf(cl, (_, cb1), (_, cb2)) -> find_array_reads_clogic cl @ find_array_reads cb1 @ find_array_reads cb2 @ find_array_reads rest
          | CWhile (cl, (_, cb)) -> find_array_reads_clogic cl @ find_array_reads cb @ find_array_reads rest
          | CFor (cinsts, cl, cinsts1, (_, cb)) -> find_array_reads cinsts @ find_array_reads_clogic cl @ find_array_reads cinsts1 @ find_array_reads cb @ find_array_reads rest
          | CReturn ex -> find_array_reads_cexpr ex
          | _ -> find_array_reads rest
    
        )
      ) in
    
      let (_,bcontents) = block
    in let arrayWrites = find_array_writes bcontents
    in let arrayReads  = (find_array_reads bcontents) in 
    
    (* This code runs for the non-array parameters*)
    let param_to_cdecl (id, t) = (match t with
      | _ -> CDecl(id ^ "_2", Shared, t, []))
    
    (* Array_idx is an option, because we don't actually know if the pointers are arrays or not. If we pass in an index (we assume this index), then it is considered an array*)
    in let rec param_to_clval (id, t, arr_idx) ind =
      match arr_idx with
      | Some arr_idx -> 
            (match (t, ind) with
        | (C.PTR b_t, _) -> CArr(param_to_clval (id, b_t, Some (arr_idx)) ind, [arr_idx])
        | (C.ARRAY (b_t, exp), Some i) -> CArr(param_to_clval (id, b_t, Some(arr_idx)) ind, [i])
        | _ -> CVar id
            )
      | None -> (match (t, ind) with
      | (C.PTR b_t, _) -> CRef (param_to_clval (id, b_t, None) ind)
      | (C.ARRAY (b_t, exp), Some i) -> CArr(param_to_clval (id, b_t, None) ind, [i])
      | _ -> CVar id
          )
    
    in let is_param_bt_not_array param =
    let (_, bt) = param in
    match bt with
    | C.PTR _ | C.ARRAY _ -> false
    | _ -> true
    
    (* in let param_names = List.map (fst) params  *)
    in let shared_back_to_global (id, t) =
      let tvar = CVar("__itertemp") in
      let tvar_as_exp = emk () (CL (CVar "__itertemp")) in
      let pointer_bt = get_pointer_bt t in
        (CFor ([CDecl ("__itertemp", Local, C.INT(C.LONG, C.SIGNED), []);
        CAssign (tvar, emk() (CConst(CInt 0)), true)],
        CCmp (tvar_as_exp, Le, emk() (CL((CVar("size_" ^ id))))),
        [CAssign (tvar, emk () (CAdd (tvar_as_exp,
                                (emk () (CConst (CInt 1))))), true)],
        ((),                              
        [(CAssign ((param_to_clval (id, t, Some (emk () (CAdd(emk () (CL(CVar("__itertemp"))), emk () (CL(CVar("lower_bound_" ^ id)))))))
                    (Some tvar_as_exp)
                    ),
                    (emk () (CL (param_to_clval (id ^ "_2", t, Some (emk () (CL(CVar("__itertemp")))))
                                  (Some tvar_as_exp)))) ,true))])))
        
    (* This is responsible for the for loop that copies from the parameter to shared memory *)
    in let global_convert_to_shared ((id, lb, ub), t) =
      let tvar = CVar("__itertemp") in
      let tvar_as_exp =
        emk () (CL (CVar "__itertemp"))
    
      (* This is to determine the original base type of the pointer, so we can convert it to an array *)
      in let pointer_bt = get_pointer_bt t
      in
      [
        CDecl ("size_" ^ id, Local, C.INT(C.LONG, C.SIGNED), []);
        CDecl ("lower_bound_" ^ id, Local, C.INT(C.LONG, C.SIGNED), []);
        CAssign(CVar("size_" ^ id), emk () (CMax(List.map expr_eval ub)), true);
        CAssign(CVar("lower_bound_" ^ id), emk () (CMin(List.map expr_eval lb)), true);
        CDecl(id ^ "_2", Shared, C.ARRAY(pointer_bt, C.VARIABLE ("size_"^ id)), []);
        (CFor ([CDecl ("__itertemp", Local, C.INT(C.LONG, C.SIGNED), []);
                CAssign (tvar, emk() (CConst(CInt 0)), true)],
                CCmp (tvar_as_exp, Le, emk() (CL((CVar("size_" ^ id))))),
                [CAssign (tvar, emk () (CAdd (tvar_as_exp,
                                        (emk () (CConst (CInt 1))))), true)],
                ((),                              
                [(CAssign ((param_to_clval (id ^ "_2", t, Some (emk () (CL(CVar("__itertemp")))))
                            (Some tvar_as_exp)
                            ),
                            (emk () (CL (param_to_clval (id, t, Some (emk () (CAdd(emk () (CL(CVar("__itertemp"))), emk () (CL(CVar("lower_bound_" ^ id)))))))
                                          (Some tvar_as_exp)))) ,true))])))
      ]
            
                 in let get_param_bounds params = 
    
                   let print_debug ((id, lbs, ubs), _) =
                    Format.fprintf Format.std_formatter "ID of %s in with LBS %a AND UBS %a\n"
                    id
                    CUDA.print_cexpr_list (List.map expr_eval lbs)
                    CUDA.print_cexpr_list (List.map expr_eval ubs) in
                    
                  
                   let param_ids = List.map (fun (id, _) -> id) params in 
                   let bounds_info = List.filter (fun (id, _, _) -> List.mem id param_ids ) (id_lbs_and_ubs (arrayReads @ arrayWrites)) in
                   let ret = List.map (fun (id, lb, ub) -> (let (_, bt) = List.find (fun (x, _) -> x = id) params in ((id, lb, ub), bt))) bounds_info 
                   in let useless = List.map print_debug ret in
                   ret
                in
                let param_bounds = get_param_bounds params in
                let param_bound_ids_bt = List.map (fun ((id, _, _), bt) -> (id, bt)) param_bounds in
                let non_array_params = List.filter is_param_bt_not_array params in
                let non_array_param_ids =  List.map (fun (id, _) -> id) (non_array_params) in
    
                (* rename_clval also changes the array accesses of the new shared arrays to subtract the lower *)
                let rec rename_clval (c: 'a clval): 'a clval = 
                 (match c with
                 | CArr(CVar id, fst_exp :: rest_exp_lst) -> if List.mem id (List.map (fun (id, _) -> id) param_bound_ids_bt)
                                                             then CArr(CVar (id ^ "_2"), emk () (CSub(rename_cexpr fst_exp, (emk () (CL(CVar("lower_bound_" ^ id)))))) :: (List.map rename_cexpr rest_exp_lst))
                                                             else CArr(CVar id, (List.map rename_cexpr (fst_exp :: rest_exp_lst)))
                 | CVar id -> CVar(if List.mem id (List.map (fun (id, _) -> id) param_bound_ids_bt) then (id ^ "_2") else id)
                 | CArr (cv, exp_lst) -> CArr(rename_clval cv, (List.map rename_cexpr exp_lst))
                 | CDeref clv -> CDeref(rename_clval clv)
                 | CRef clv -> CRef(rename_clval clv)
                 )
                  and rename_cexpr (exp: 'a cexpr): 'a cexpr =
                    emk ()
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
               and rename_cinstr (code_instr: 'a cinstr): 'a cinstr =  
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
               and rename_cblock (a, block) =
                 (a, List.map rename_cinstr block) in
                 let (a, bl) = let (_, block) = block in rename_cblock (emk () (List.map erase_instr block)) in
                 let arrs =
                  (* let _ = print_string (String.concat ", " (List.map (fun ((id, _, _) , _) -> id) (get_param_bounds params))) in *)
                  (t, id, params, (a, 
                                   List.flatten (List.map global_convert_to_shared (param_bounds))
                                  @ bl
                                  @ (List.map shared_back_to_global (param_bound_ids_bt))), b)
                  in arrs
    




(*
type cfunc = string * (id * Cabs.base_type) list * cblock * bool
(* true = is a kernel *)

type cprog = (id * Cabs.base_type * mem) list * cfunc list
 *)

let branch_distribution_prog (cutoff: int) ((globals, funcs): 'a cprog) =
  let bdf (rt, name, args, code, kernel) =
    global_to_shared (rt, name, args, branch_distribution cutoff code, kernel)
  in
  (globals, List.map bdf funcs)
   
let branch_distribution_mult (prog: 'a cprog) =
  let bdp c = branch_distribution_prog c prog in
  [bdp 0; bdp 10]
  
(** transformations = [optimization_function, optimization_name, required?]
    return [([performed_optimizations], code)]

*)


(* 
let rec apply_optimizations (block:cblock) (transformations: (CUDA_Types.cinstr list -> CUDA_Types.cinstr list * string * bool) list) = 
  let add_opt item (opt_fun
  ) (opt_name) = let (performed_optimizations, code) = item in ([performed_optimizations @ [opt_name], opt_fun code])
  in  
  let rec helper (blocks) (transformations: (CUDA_Types.cinstr list -> CUDA_Types.cinstr list * string * bool) list) = 
    match transformations with 
    | [] -> blocks
    | transform :: rest -> (
      match transform with 
        | (opt_fun, opt_name, true) -> List.map (add_opt opt_fun opt_name) (blocks)
        | _ -> blocks
    )
  in helper [block] transformations *)





let cf_func (rt, name, args, body, is_kernel) =
  global_to_shared (rt, name, args, branch_distribution 3 body, is_kernel)

let cf_prog (decls, funcs) =
  (decls, List.map cf_func funcs)

  (*
let input_file = Sys.argv.(1)

let _ =
  match Frontc.parse_file input_file stdout with
  | Frontc.PARSING_ERROR -> failwith "parse error"
  | Frontc.PARSING_OK ccode ->
     let cuda = CUDA.cuda_of_file () input_file ccode in
     let folded = cf_prog cuda in
     CUDA.print_cprog Format.std_formatter folded
  
   *)
