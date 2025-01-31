
type token = 
  | WHILE of (string * int)
  | VOLATILE
  | VOID
  | UNSIGNED
  | UNION
  | TYPEDEF
  | TILDE
  | TEMPLATE
  | SWITCH of (string * int)
  | SUP_SUP_EQ of (string * int)
  | SUP_SUP
  | SUP_EQ
  | SUP
  | STRUCT
  | STATIC
  | STAR_EQ of (string * int)
  | STAR
  | SLASH_EQ of (string * int)
  | SLASH
  | SIZEOF
  | SIGNED
  | SHORT
  | SHARED
  | SEMICOLON of (string * int)
  | RPAREN of (string * int)
  | RETURN of (string * int)
  | RESTRICT
  | REGISTER
  | RBRACKET of (string * int)
  | RBRACE of (string * int)
  | QUEST of (string * int)
  | PLUS_PLUS
  | PLUS_EQ of (string * int)
  | PLUS
  | PIPE_PIPE
  | PIPE_EQ of (string * int)
  | PIPE
  | PERCENT_EQ of (string * int)
  | PERCENT
  | NAMED_TYPE of (string)
  | MINUS_MINUS
  | MINUS_EQ of (string * int)
  | MINUS
  | LPAREN of (string * int)
  | LONG
  | LBRACKET of (string * int)
  | LBRACE of (string * int)
  | INT
  | INLINE
  | INF_INF_EQ of (string * int)
  | INF_INF
  | INF_EQ
  | INF
  | IF of (string * int)
  | IDENT of (string)
  | GOTO of (string * int)
  | GNU_ATTRS of (Cabs.gnu_attrs)
  | GLOBAL
  | FOR of (string * int)
  | FLOAT
  | EXTERN
  | EXTENSION
  | EXCLAM_EQ
  | EXCLAM
  | EQ_EQ
  | EQ of (string * int)
  | EOF
  | ENUM
  | ELSE of (string * int)
  | ELLIPSIS of (string * int)
  | DOUBLE
  | DOT
  | DO of (string * int)
  | DEFAULT of (string * int)
  | CST_STRING of (string)
  | CST_INT of (string)
  | CST_FLOAT of (string)
  | CST_CHAR of (string)
  | CONTINUE of (string * int)
  | CONST
  | COMPLEX
  | COMMA of (string * int)
  | COLON of (string * int)
  | CIRC_EQ of (string * int)
  | CIRC
  | CHAR
  | CASE of (string * int)
  | BUILTIN_TYPE of (string)
  | BREAK of (string * int)
  | BOOL
  | AUTO
  | ATTRIBUTE
  | ASM
  | ARROW
  | AND_EQ of (string * int)
  | AND_AND
  | AND
