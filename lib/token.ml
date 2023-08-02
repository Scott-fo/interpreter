type t =
  | ILLEGAL
  | EOF
  (* Identifiers and literals *)
  | IDENT of string
  | INT of string
  (* Operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  (* Delimiters *)
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  (* Keywords *)
  | FUNCTION
  | LET
[@@deriving show, eq]

let lookup_ident = function "fn" -> FUNCTION | "let" -> LET | str -> IDENT str
