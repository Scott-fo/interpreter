(** Parser Definition*)

type t

(** Methods *)

val init : Lexer.t -> t
val next_token : t -> t
