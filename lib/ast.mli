module Identifier : sig
  type t = { identifier : string }

  val show : t -> string
  val equal : t -> t -> bool
end

module Expression : sig
  type t = IDENTIFIER of Identifier.t

  val show : t -> string
  val equal : t -> t -> bool
end

module Statement : sig
  type t = LET of { name : Identifier.t; value : Expression.t }

  val show : t -> string
  val equal : t -> t -> bool
end

module Program : sig
  type t = { statements : Statement.t list }

  val show : t -> string
  val equal : t -> t -> bool
end

module Node : sig
  type t =
    | PROGRAM of Program.t
    | STATEMENT of Statement.t
    | EXPRESSION of Expression.t
end
