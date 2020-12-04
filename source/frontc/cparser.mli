type token =
  | IDENT of (string)
  | CST_CHAR of (string)
  | CST_INT of (string)
  | CST_FLOAT of (string)
  | CST_STRING of (string)
  | NAMED_TYPE of (string)
  | GNU_ATTRS of (Cabs.gnu_attrs)
  | EOF
  | CHAR
  | INT
  | DOUBLE
  | FLOAT
  | VOID
  | ENUM
  | STRUCT
  | TYPEDEF
  | UNION
  | SIGNED
  | UNSIGNED
  | LONG
  | SHORT
  | VOLATILE
  | EXTERN
  | STATIC
  | CONST
  | AUTO
  | REGISTER
  | RESTRICT
  | TEMPLATE
  | GLOBAL
  | SHARED
  | SIZEOF
  | ASM
  | EQ of (string * int)
  | PLUS_EQ of (string * int)
  | MINUS_EQ of (string * int)
  | STAR_EQ of (string * int)
  | SLASH_EQ of (string * int)
  | PERCENT_EQ of (string * int)
  | AND_EQ of (string * int)
  | PIPE_EQ of (string * int)
  | CIRC_EQ of (string * int)
  | INF_INF_EQ of (string * int)
  | SUP_SUP_EQ of (string * int)
  | ARROW
  | DOT
  | EQ_EQ
  | EXCLAM_EQ
  | INF
  | SUP
  | INF_EQ
  | SUP_EQ
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | TILDE
  | AND
  | PIPE
  | CIRC
  | EXCLAM
  | AND_AND
  | PIPE_PIPE
  | INF_INF
  | SUP_SUP
  | PLUS_PLUS
  | MINUS_MINUS
  | RPAREN of (string * int)
  | LPAREN of (string * int)
  | RBRACE of (string * int)
  | LBRACE of (string * int)
  | LBRACKET of (string * int)
  | RBRACKET of (string * int)
  | COLON of (string * int)
  | SEMICOLON of (string * int)
  | COMMA of (string * int)
  | ELLIPSIS of (string * int)
  | QUEST of (string * int)
  | BREAK of (string * int)
  | CONTINUE of (string * int)
  | GOTO of (string * int)
  | RETURN of (string * int)
  | SWITCH of (string * int)
  | CASE of (string * int)
  | DEFAULT of (string * int)
  | WHILE of (string * int)
  | DO of (string * int)
  | FOR of (string * int)
  | IF of (string * int)
  | ELSE of (string * int)
  | ATTRIBUTE
  | EXTENSION
  | INLINE

val interpret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cabs.definition list
val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cabs.definition list
