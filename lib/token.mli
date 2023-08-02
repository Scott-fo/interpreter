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

(** Definitions from deriving *)

val show : t -> string
val equal : t -> t -> bool

(** Get identifier from string*)

val lookup_ident : string -> t
