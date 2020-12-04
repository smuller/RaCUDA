(* Quentin Carbonneaux - 2016-2017 *)
(* Van Chan Ngo - 2017 *)
{
open IMP_Grammar

let kw = Hashtbl.create 23
let kwp = Hashtbl.create 23
let () =
  List.iter (fun (k,t) -> Hashtbl.add kw k t)
  [ "var", TVAR
  ; "true", TTRUE
  ; "false", TFALSE
  ; "random", TRANDOM
  ; "skip", TSKIP
  ; "not", TNOT
  ; "and", TAND
  ; "or", TOR
  ; "focus", TFOCUS
  ; "ber", TBER                      (* Bernoulli distribution - ber(pa, pb) *)
  ; "bin", TBIN                      (* Binomial distribution - bin(n, pa, pb) *)
  ; "geo", TGEO                      (* Geometric distribution - geo(pa, pb) *)
  ; "nbin", TNBIN                    (* Negative binomial distribution - nbin(r, pa, pb) *)
  ; "pois", TPOIS                    (* Poisson distribution - pois(la, lb) *) 
  ; "hyper", THYPER                  (* Hyper-geometric distribution - hyper(n, r, m) *)
  ; "unif", TUNIF                    (* Uniform distribution - unif(a, b) *)
  ; "dist", TDIST                    (* Arbitrary distribution *)
  ];
  List.iter (fun (k,t) -> Hashtbl.add kwp k t)
  [ "break", (fun p -> TBREAK p)
  ; "return", (fun p -> TRETURN p)
  ; "assume", (fun p -> TASSUME p)
  ; "if", (fun p -> TIF p)
  ; "prob", (fun p -> TPROBIF p)
  ; "while", (fun p -> TWHILE p)
  ; "loop", (fun p -> TLOOP p)
  ; "def", (fun p -> TDEF p)
  ; "end", (fun p -> TEND p)
  ; "then", (fun p -> TTHEN p)
  ; "else", (fun p -> TELSE p)
  ; "do", (fun p -> TDO p)
  ; "weaken", (fun p -> TWEAK p)
  ; "pass", (fun p -> TPASS p)
  ; "tick", (fun p -> TTICK p)
  ]

let pos { Lexing.lex_start_p = start; _ } =
  { Types.pos_file = start.Lexing.pos_fname
  ; pos_line = start.Lexing.pos_lnum
  ; pos_bol = start.Lexing.pos_bol
  ; pos_char = start.Lexing.pos_cnum
  }

let lvl = ref 0
let buf0 = ref []
let buf1 = ref []

let push t =
  buf0 := t :: !buf0

let newline lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with Lexing.
      pos_lnum = pos.Lexing.pos_lnum + 1;
      pos_bol = pos.Lexing.pos_cnum;
    }

let indent ?lvl' lexbuf =
  let s = Lexing.lexeme lexbuf in
  let p = pos lexbuf in
  let lvl' =
    match lvl' with
    | Some l -> l
    | None ->
      let rec go ns nt i =
        if i = String.length s
        then ns/4 + nt
        else match s.[i] with
        | ' ' -> go (ns+1) nt (i+1)
        | '\t' -> go ns (nt+1) (i+1)
        | _ -> go ns nt (i+1)
      in go 0 0 0
  in
  for i = lvl' to !lvl - 1
    do push (TDE p) done;
  for i = !lvl to lvl' - 1
    do push (TIN p) done;
  lvl := lvl'

}

let first = ['a'-'z' 'A'-'Z' '_']
let ident = first (first | ['0'-'9'])*
let space = [ ' ' '\t' ]
let eol = space* ('#' [^'\n']*)? '\n'

rule line = parse
  | space* eol { newline lexbuf; line lexbuf }
  | space* eof { indent ~lvl':0 lexbuf; push TEOF }
  | space*     { indent lexbuf; token lexbuf }

and skip = parse
  | space* eol { newline lexbuf; skip lexbuf }
  | space*     { token lexbuf }

and token = parse
  | eol        { newline lexbuf; push TNL; line lexbuf }
  | space      { token lexbuf }
  | '\\' '\n'  { newline lexbuf; skip lexbuf }

  | ['0'-'9']+ { push (TNUM (int_of_string (Lexing.lexeme lexbuf))) }

  | ":" eol    { newline lexbuf; push (TCOLON (pos lexbuf));
                 line lexbuf }
  | ":"        { push (TCOLON (pos lexbuf)) }
  | "," eol    { newline lexbuf; push TCOMMA; skip lexbuf }
  | ","        { push TCOMMA }

  | ";"        { push TSEMI }
  | "("        { push (TLPAR (pos lexbuf)) }
  | ")"        { push TRPAR }
  | "<="       { push TLEQ }
  | "<"        { push TLT }
  | ">="       { push TGEQ }
  | ">"        { push TGT }
  | "="        { push TEQ }
  | "!="       { push TNE }
  | "+"        { push TADD }
  | "-"        { push TSUB }
  | "*"        { push TMUL }
  | ident      { let id = Lexing.lexeme lexbuf in
                 let p = pos lexbuf in
                 push (
                   try Hashtbl.find kw id with Not_found ->
                   try Hashtbl.find kwp id p with Not_found ->
                   TIDENT (id, p)
                 ) }

{

let lexf = ref line

let init lexbuf fname =
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname };
  lvl := 0;
  buf0 := [];
  buf1 := [];
  lexf := line

let rec lex lexbuf =
  buf1 := List.rev_append !buf0 !buf1;
  buf0 := [];
  match !buf1 with
  | token :: buf1' ->
    buf1 := buf1';
    token
  | [] ->
    !lexf lexbuf;
    lexf := token;
    lex lexbuf

}
