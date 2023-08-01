type t =
  | ILLEGAL
  | EOF
  | IDENT of string
  | INT of string
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
[@@deriving show, eq]

let lookup_ident = function "fn" -> FUNCTION | "let" -> LET | str -> IDENT str
