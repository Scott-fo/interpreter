module Identifier = struct
  type t = { identifier : string } [@@deriving eq, show]
end

module Expression = struct
  type t = IDENTIFIER of Identifier.t [@@deriving eq, show]
end

module Statement = struct
  type t = LET of { name : Identifier.t; value : Expression.t }
  [@@deriving eq, show]
end

module Program = struct
  type t = { statements : Statement.t list } [@@deriving eq, show]
end

module Node = struct
  type t =
    | PROGRAM of Program.t
    | STATEMENT of Statement.t
    | EXPRESSION of Expression.t
end
