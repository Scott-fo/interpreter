(** Lexer Definition *)

type t

(** Create a new lexer from a string input *)

val init : string -> t

(** Move to the next token. Returns a new lexer and the token (optional)*)

val next_token : t -> t * Token.t option
