(* Van Chan Ngo - 2017 *)
{
open CS_Interop_Grammar
open CS_Interop_Types

let kw = Hashtbl.create 23

let () =
  List.iter (fun (k,t) -> Hashtbl.add kw k t)
  [ "true", TTRUE
  ; "false", TFALSE
  ; "random", TRANDOM
  ; "not", TNOT
  ; "and", TAND
  ; "or", TOR
  ; "global", TGLOBAL
  ; "local", TLOCAL
  ; "return", TRETURN
  ; "func", TFUNC
  ; "goto", TGOTO
  ; "assert", TASSERT
  ; "assume", TASSUME
  ; "focus", TFOCUS
  ; "tick", TTICK
  ; "ber", TBER                      (* Bernoulli distribution - ber(pa, pb) *)
  ; "bin", TBIN                      (* Binomial distribution - bin(n, pa, pb) *)
  ; "geo", TGEO                      (* Geometric distribution - geo(pa, pb) *)
  ; "nbin", TNBIN                    (* Negative binomial distribution - nbin(r, pa, pb) *)
  ; "pois", TPOIS                    (* Poisson distribution - pois(la, lb) *) 
  ; "hyper", THYPER                  (* Hyper-geometric distribution - hyper(n, r, m) *)
  ; "unif", TUNIF                    (* Uniform distribution - unif(a, b) *)
  ; "dist", TDIST                    (* Arbitrary distribution *)
  ; "prob", TPROBIF                  (* Probabilistic branching *)
  ]

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { 
    pos with 
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let first = ['a'-'z' 'A'-'Z' '_']
let ident = first (first | ['0'-'9'])*

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '#' [^ '\n']*   { token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | ['0'-'9']+ as s { TNUM (int_of_string s) }
  | ident as s ':'  { try Hashtbl.find kw s with Not_found -> TLABEL s }
  | ident as s      { try Hashtbl.find kw s with Not_found -> TIDENT s }
  | "<="            { TLEQ }
  | ">="            { TGEQ }
  | "!="            { TNEQ }
  | '<'             { TLT }
  | '>'             { TGT }
  | '='             { TEQ }
  | '*'             { TMUL }
  | '+'             { TADD }
  | '-'             { TSUB }
  | '('             { TLPAR }
  | ')'             { TRPAR }
  | ';'             { TSEMI }
  | ','             { TCOMMA }
  | '['             { TLSQE }
  | ']'             { TRSQE }
  | eof             { TEOF }

{

let init lexbuf fname =
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }

(* Parsing functions for CFG, returns a list of globals and a list of cfg_funcs *)
let cfg_file ic = 
  let lexbuf = Lexing.from_channel ic in init lexbuf "";
  try 
    let globals, cfg_funcs = CS_Interop_Grammar.cfg_file token lexbuf in
    (* fill predecessors for each block *)
    let fill_preds_func f = { f with fbody = CS_Utils.fill_preds f.fbody } in
    (globals, List.map fill_preds_func cfg_funcs)

  with Parsing.Parse_error ->
    let startp = Lexing.lexeme_start_p lexbuf in
      Format.eprintf "%s:%i:%i: syntax error near '%s'@."
      startp.Lexing.pos_fname
      startp.Lexing.pos_lnum
      (startp.Lexing.pos_cnum - startp.Lexing.pos_bol + 1)
      (Lexing.lexeme lexbuf);
    raise Utils.Error

}
