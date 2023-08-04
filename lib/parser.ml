type t = { lexer : Lexer.t; curr : Token.t option; peek : Token.t option }

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; peek; curr = parser.peek }

let init lexer = { lexer; curr = None; peek = None } |> next_token |> next_token
let parse_let_statement _parser = failwith "NYI"

let parse_statement parser =
  match parser.curr with
  | Some Token.LET -> parse_let_statement parser
  | _ -> failwith "NYI"

let parse_program parser =
  let rec aux parser statements =
    match parser.curr with
    | None -> Ok (parser, List.rev statements)
    | Some _ -> (
        match parse_statement parser with
        | Error e -> Error e
        | Ok (parser, statement) ->
            aux (next_token parser) (statement :: statements))
  in
  match aux parser [] with
  | Ok (_, statements) -> Ok { Ast.Program.statements }
  | Error e -> Error e

module Test = struct
  let%test_unit "let statements" =
    let input =
      {|
        let x = 5;
        let y = 10;
        let foobar = 69;
        |}
    in
    let lexer = Lexer.init input in
    let parser = init lexer in
    match parser.curr with
    | None -> ()
    | Some c -> (
        Stdlib.print_endline (Token.show c);
        let expected_output =
          {
            Ast.Program.statements =
              [
                Ast.Statement.LET
                  {
                    name = { Ast.Identifier.identifier = "x" };
                    value =
                      Ast.Expression.IDENTIFIER
                        { Ast.Identifier.identifier = "5" };
                  };
                Ast.Statement.LET
                  {
                    name = { Ast.Identifier.identifier = "y" };
                    value =
                      Ast.Expression.IDENTIFIER
                        { Ast.Identifier.identifier = "10" };
                  };
                Ast.Statement.LET
                  {
                    name = { Ast.Identifier.identifier = "foobar" };
                    value =
                      Ast.Expression.IDENTIFIER
                        { Ast.Identifier.identifier = "69" };
                  };
              ];
          }
        in
        match parse_program parser with
        | Error e -> failwith e
        | Ok program -> assert (Ast.Program.equal expected_output program))
end
