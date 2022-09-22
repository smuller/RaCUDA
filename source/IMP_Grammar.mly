/* Quentin Carbonneaux - 2016-2017 */
/* Van Chan Ngo - 2017 */

%{
  open Types
  open IMP_Types

  let mkb (startp, b, endp) : unit IMP_Types.block =
    { b_start_p = startp
    ; b_end_p = endp
    ; b_body = b
    ; annot = ()
    }
  let b b_start_p blk = { blk with b_start_p }
  let belse p b = mkb (b.b_end_p, [], b.b_end_p)

%}

%token TVAR TTRUE TFALSE TRANDOM TSKIP 
%token TDIST TBER TBIN TGEO TNBIN TPOIS THYPER TUNIF
%token TNOT TAND TOR TFOCUS TSEMI TCOMMA
%token TRPAR TLEQ TLT TGEQ TGT TEQ TNE TADD TSUB TMUL
%token TEOF TNL
%token <Types.position> TBREAK TRETURN TASSUME TIF TELSE TWHILE TLOOP TLPAR TPROBIF
%token <Types.position> TDEF TEND TTHEN TDO TWEAK TIN TDE TCOLON TPASS TTICK
%token <(Types.id * Types.position)> TIDENT
%token <int> TNUM

%left TOR
%left TAND
%nonassoc TNOT
%nonassoc TEQ TNE TGEQ TGT TLEQ TLT
%left TADD TSUB
%left TMUL

%type <Types.id list * (unit Types.free_expr, unit IMP_Types.block) Types.func_ list> file
%type <unit Types.expr_> expr_
%type <unit Types.expr> expr
%type <unit Types.expr> exprr
%type <unit Types.logic> logic
%type <unit IMP_Types.instr * Types.position> simpl
%type <unit IMP_Types.instr * Types.position> instr
%type <unit Types.free_expr> free
%type <unit IMP_Types.block> block
%type <Types.id> ident
%start file
%%

file: varopt rfuncs TEOF { $1, List.rev $2 }

ident: TIDENT { fst $1 }

expr_:
  | ident                                                                     { EVar $1 }
  
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

  | TDIST TLPAR TIDENT TCOMMA TNUM TCOMMA TNUM TCOMMA TNUM TCOMMA TNUM TRPAR  { EDist (fst $3, $5, $7, $9, $11) }
  
  | TSUB expr                                                                 { ESub (mk () (ENum 0), $2) }

  | expr TADD expr                                                            { EAdd ($1, $3) }
  
  | expr TSUB expr                                                            { ESub ($1, $3) }
  
  | expr TMUL expr                                                            { EMul ($1, $3) }

  | TLPAR expr_ TRPAR                                                          { $2 }

expr:
| expr_   { mk () $1 }

exprr:
  | TRANDOM { mk () ERandom }
  | expr    { $1 }

prob_expr:
  | TLPAR TNUM TCOMMA TNUM TRPAR { EProb ($2, $4) }

logic:
    TTRUE            { LTrue }
  | TFALSE           { LFalse }
  | TRANDOM          { LRandom }
  | expr TLEQ expr   { LCmp ($1, Le, $3) }
  | expr TLT expr    { LCmp ($1, Lt, $3) }
  | expr TGEQ expr   { LCmp ($1, Ge, $3) }
  | expr TGT expr    { LCmp ($1, Gt, $3) }
  | expr TEQ expr    { LCmp ($1, Eq, $3) }
  | expr TNE expr    { LCmp ($1, Ne, $3) }
  | logic TAND logic { LAnd ($1, $3) }
  | logic TOR logic  { LOr ($1, $3) }
  | TNOT logic       { LNot $2 }
  | TLPAR logic TRPAR { $2 }

rids1:
    ident              { [$1] }
  | rids1 TCOMMA ident { $3 :: $1 }

ids:
    /* empty */ { [] }
  | rids1       { List.rev $1 }

simpl:
  | TBREAK TNL                                                      { IBreak, $1 }
  | TWEAK TNL                                                       { IWeaken, $1 }
  | TASSUME logic TNL                                               { IAssume $2, $1 }
  | TIDENT TEQ exprr TNL                                            { IAssign (fst $1, $3), snd $1 }
  | TTICK TNUM TNL                                                  { ITick $2, $1 }
  | TRETURN TIDENT TNL                                              { IReturn [(fst $2)], $1 }
  | TRETURN TLPAR rids1 TRPAR TNL                                   { IReturn (List.rev $3), $1 }
  
instr:
  | simpl                                                           { $1 }
  
  | TIDENT TLPAR TRPAR TNL                                          { ICall (fst $1), snd $1 }
  | TIDENT TLPAR rids1 TRPAR TNL                                    { ICallArg ([], (fst $1), $3), snd $1 }
  | TIDENT TEQ TIDENT TLPAR TRPAR TNL                               { ICallArg ([fst $1], (fst $3), []), snd $1 }
  | TIDENT TEQ TIDENT TLPAR rids1 TRPAR TNL                         { ICallArg ([fst $1], (fst $3), (List.rev $5)), snd $1 }
  | TLPAR rids1 TRPAR TEQ TIDENT TLPAR TRPAR TNL                    { ICallArg ((List.rev $2), (fst $5), []), snd $5 }
  | TLPAR rids1 TRPAR TEQ TIDENT TLPAR rids1 TRPAR TNL              { ICallArg ((List.rev $2), (fst $5), (List.rev $7)), snd $5 }

  | TWHILE logic TCOLON block                                       { IWhile ($2, b $1 $4), $1 }
  | TLOOP TCOLON block                                              { ILoop (b $2 $3), $1 }
  | TIF logic TCOLON block                                          { IIf ($2, b $3 $4, belse $3 $4), $1 }
  | TIF logic TCOLON block TELSE TCOLON block                       { IIf ($2, b $3 $4, b $6 $7), $1 }
  | TPROBIF prob_expr TCOLON block                                  { IProbIf ($2, b $3 $4, belse $3 $4), $1 }
  | TPROBIF prob_expr TCOLON block TELSE TCOLON block               { IProbIf ($2, b $3 $4, b $6 $7), $1 }


rinstrs1:
    instr          { [$1] }
  | rinstrs1 instr { $2 :: $1 }

block:
  | TIN TPASS TNL TDE { mkb ($1, [], $4) }
  | TPASS TNL         { mkb ($1, [], $1) }
  | simpl             { mkb (snd $1, [$1], snd $1) }
  | TIN rinstrs1 TDE  { mkb ($1, List.rev $2, $3) }

rfrees:
    free               { [$1] }
  | rfrees TCOMMA free { $3 :: $1 }

frees:
    /* empty */ { [] }
  | rfrees      { List.rev $1 }

free:
    expr                     { FBase $1 }
  | TIDENT TLPAR frees TRPAR { FApply (fst $1, $3, snd $1) }
  | expr TGEQ expr           { FApply (">=", [FBase $1; FBase $3]
                                      , Utils.dummy_position) }

varopt:
    /* empty */  { [] }
  | TVAR ids TNL { $2 }

focusopt:
    /* empty */      { [] }
  | TFOCUS frees TNL { $2 }

func:
  TDEF ident TLPAR ids TRPAR TCOLON
  TIN varopt focusopt rinstrs1 TDE
  { { fun_name = $2; fun_vars = $8
    ; fun_focus = $9; fun_body = mkb ($6, List.rev $10, $11)
    ; fun_start_p = $1; fun_end_p = $11; fun_args = $4 }
  }

rfuncs:
    func        { [$1] }
  | rfuncs func { $2 :: $1 }

