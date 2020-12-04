/* Van Chan Ngo - 2017 */

%{
  open Types
  open CS_Interop_Types

  let mk_cfgblocks l =
    let h = Hashtbl.create 101 in
    
    let bnum b =
      try Hashtbl.find h b
      with Not_found ->
        Printf.eprintf "Parse error: no definition for block %s\n" b;
        raise Parsing.Parse_error 
    in
    
    let mk_cfgblock (_, i, j) =
      let j = match j with
              | `Goto l -> JJmp (List.map bnum l)
              | `Ret v -> JRet v
              | `Retx s -> JRetVar [s]
              | `Retl l -> JRetVar l
      in
      {
        bpreds = []; 
        binsts = i; 
        bjump = j
      } 
    in

    List.iteri (fun i (b,_,_) -> Hashtbl.add h b i) l;
    List.map mk_cfgblock l |> Array.of_list

  let mk_cfgfunc (cfgname, args, cfglocs) cfgbody =
    let res = {
      fname = cfgname; 
      flocs = cfglocs; 
      fargs = args;
      fbody = cfgbody
    }
    in 
    res
%}

%token TTRUE TFALSE TRANDOM TNOT TAND TOR TGLOBAL TLOCAL
%token TRETURN TFUNC TGOTO TASSERT TASSUME TTICK TFOCUS
%token TLEQ TGEQ TNEQ TLT TGT TEQ TMUL TADD TSUB TLPAR TRPAR TSEMI TCOMMA TEOF TLSQE TRSQE
%token TDIST TBER TBIN TGEO TNBIN TPOIS THYPER TUNIF TPROBIF
%token <Types.id> TIDENT
%token <int> TNUM
%token <Types.id> TLABEL

%left TOR
%left TAND
%nonassoc TNOT
%nonassoc TEQ TNEQ TGEQ TGT TLEQ TLT
%left TADD TSUB
%left TMUL

%start cfg_file
%type <Types.id list * CS_Interop_Types.csblock array CS_Interop_Types.csfunc list> cfg_file
%%

cfg_file: globals cfuncs TEOF { $1, $2 }

names1: 
    TIDENT                { [$1] } 
  | names1 TCOMMA TIDENT  { $3 :: $1 }

names: 
    /* empty */         { [] } 
  | names1              { List.rev $1 }

globals: 
    /* empty */               { [] }
  | TGLOBAL TLPAR names TRPAR { $3 }

proto:
    TFUNC TIDENT TLPAR names TRPAR                          { $2, $4, [] } 
  | TFUNC TIDENT TLPAR names TRPAR TLOCAL TLPAR names TRPAR { $2, $4, $8 }

cfunc: proto blocks                                   { mk_cfgfunc $1 (mk_cfgblocks $2) }

cfuncs: 
    /* empty */         { [] } 
  | cfunc TSEMI cfuncs  { $1 :: $3 }

expr:
  | TIDENT                                                                    { EVar $1 }
  | TNUM                                                                      { ENum $1 }
  | TBER TLPAR TNUM TCOMMA TNUM TRPAR                                         { EBer ($3, $5) }
  | TBIN TLPAR TNUM TCOMMA TNUM TCOMMA TNUM TRPAR                             { EBin ($3, $5, $7) }
  | TGEO TLPAR TNUM TCOMMA TNUM TRPAR                                         { EGeo ($3, $5) }
  | TNBIN TLPAR TNUM TCOMMA TNUM TCOMMA TNUM TRPAR                            { ENbin ($3, $5, $7) }
  | TPOIS TLPAR TNUM TCOMMA TNUM TRPAR                                        { EPois ($3, $5) }
  | THYPER TLPAR TNUM TCOMMA TNUM TCOMMA TNUM TRPAR                           { EHyper ($3, $5, $7) }
  | TUNIF TLPAR TNUM TCOMMA TNUM TRPAR                                        { EUnif ($3, $5) }
  | TUNIF TLPAR TSUB TNUM TCOMMA TNUM TRPAR                                   { EUnif ((0 - $4), $6) }
  | TUNIF TLPAR TNUM TCOMMA TSUB TNUM TRPAR                                   { EUnif ($3, (0 - $6)) }
  | TUNIF TLPAR TSUB TNUM TCOMMA TSUB TNUM TRPAR                              { EUnif ((0 - $4), (0 - $7)) }
  | TDIST TLPAR TIDENT TCOMMA TNUM TCOMMA TNUM TCOMMA TNUM TCOMMA TNUM TRPAR  { EDist ($3, $5, $7, $9, $11) }
  | TSUB expr                                                                 { ESub (ENum 0, $2) }
  | expr TADD expr                                                            { EAdd ($1, $3) }
  | expr TSUB expr                                                            { ESub ($1, $3) }
  | expr TMUL expr                                                            { EMul ($1, $3) }
  | TLPAR expr TRPAR                                                          { $2 }

exprr:
  | TRANDOM { ERandom }
  | expr    { $1 }

prob_expr:
  | TLPAR TNUM TCOMMA TNUM TRPAR { EProb ($2, $4) }

logic:
    TTRUE               { LTrue }
  | TFALSE              { LFalse }
  | TRANDOM             { LRandom }
  | expr TLEQ expr      { LCmp ($1, Le, $3) }
  | expr TLT expr       { LCmp ($1, Lt, $3) }
  | expr TGEQ expr      { LCmp ($1, Ge, $3) }
  | expr TGT expr       { LCmp ($1, Gt, $3) }
  | expr TEQ expr       { LCmp ($1, Eq, $3) }
  | expr TNEQ expr      { LCmp ($1, Ne, $3) }
  | logic TAND logic    { LAnd ($1, $3) }
  | logic TOR logic     { LOr ($1, $3) }
  | TNOT logic          { LNot $2 }
  | TLPAR logic TRPAR   { $2 }

simpl:
    TTICK TNUM                                        { CSITick $2 }
  | TASSERT logic                                     { CSIAssert $2 }
  | TASSUME logic                                     { CSIAssume $2 }
  | TIDENT TEQ exprr                                  { CSIAssign ($1, $3) }
  | TPROBIF prob_expr                                 { CSProbIf $2 }

insn:
    simpl                                             { $1 }
  | TIDENT TLPAR TRPAR                                { CSICall $1 }  
  | TIDENT TLPAR names1 TRPAR                         { CSICallArg ([], $1, List.rev $3) }
  | TIDENT TEQ TIDENT TLPAR TRPAR                     { CSICallArg ([$1], $3, []) }
  | TIDENT TEQ TIDENT TLPAR names1 TRPAR              { CSICallArg ([$1], $3, List.rev $5) }

  | TLSQE names1 TRSQE TEQ TIDENT TLPAR TRPAR         { CSICallArg (List.rev $2, $5, []) }
  | TLSQE names1 TRSQE TEQ TIDENT TLPAR names1 TRPAR  { CSICallArg (List.rev $2, $5, List.rev $7) }

insns: 
    /* empty */           { [] } 
  | insn insns            { $1 :: $2 }

block: TLABEL insns jump  { ($1, $2, $3) }

blist: 
    /* empty */           { [] } 
  | block blist           { $1 :: $2 }

blocks: TLPAR blist TRPAR { $2 }

jump: 
      TGOTO names1                { `Goto (List.rev $2) }
    | TRETURN                     { `Ret 0 }
    | TRETURN TIDENT              { `Retx $2 }
    | TRETURN TLSQE names1 TRSQE  { `Retl (List.rev $3) }


