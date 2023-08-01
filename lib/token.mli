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

(** Definitions from deriving *)

val show : t -> string
val equal : t -> t -> bool

(** Get identifier from string*)

val lookup_ident : string -> t
