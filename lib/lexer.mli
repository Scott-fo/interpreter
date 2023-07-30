type t
(** Lexer Definition *)

val init : string -> t
(** Create a new lexer from a string input *)

val next_token : t -> t * Token.t option
(** Move to the next token. Returns a new lexer and the token (optional)*)
