module Node : sig
  type t = PROGRAM | STATEMENT | EXPRESSION
end

module Identifier : sig
  type t = { value : string }

  val show : t -> string
  val equal : t -> t -> bool
end

module Expression : sig
  type t = IDENTIFIER of Identifier.t

  val show : t -> string
  val equal : t -> t -> bool
end

module Statement : sig
  type t = LET of { identifier : Identifier.t; value : Expression.t }

  val show : t -> string
  val equal : t -> t -> bool
end

module Program : sig
  type t = { statements : Statement.t list }

  val show : t -> string
  val equal : t -> t -> bool
end
