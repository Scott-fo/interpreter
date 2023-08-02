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
  | EQ
  | NOT_EQ
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
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
[@@deriving show, eq]

let lookup_ident = function
  | "fn" -> FUNCTION
  | "let" -> LET
  | "true" -> TRUE
  | "false" -> FALSE
  | "if" -> IF
  | "else" -> ELSE
  | "return" -> RETURN
  | str -> IDENT str
