module Node = struct
  type t = PROGRAM | STATEMENT | EXPRESSION
end

module Identifier = struct
  type t = { value : string } [@@deriving eq, show]
end

module Expression = struct
  type t = IDENTIFIER of Identifier.t [@@deriving eq, show]
end

module Statement = struct
  type t = LET of { identifier : Identifier.t; value : Expression.t }
  [@@deriving eq, show]
end

module Program = struct
  type t = { statements : Statement.t list } [@@deriving eq, show]
end
