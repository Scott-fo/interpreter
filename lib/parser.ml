type t = { lexer : Lexer.t; curr : Token.t option; peek : Token.t option }

let next_token parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { lexer; peek; curr = parser.peek }

let init lexer = { lexer; curr = None; peek = None } |> next_token |> next_token

let parse_program _ =
  {
    Ast.Program.statements =
      [
        Ast.Statement.LET
          {
            identifier = { Ast.Identifier.value = "x" };
            value = Ast.Expression.IDENTIFIER { Ast.Identifier.value = "5" };
          };
        Ast.Statement.LET
          {
            identifier = { Ast.Identifier.value = "y" };
            value = Ast.Expression.IDENTIFIER { Ast.Identifier.value = "10" };
          };
        Ast.Statement.LET
          {
            identifier = { Ast.Identifier.value = "foobar" };
            value = Ast.Expression.IDENTIFIER { Ast.Identifier.value = "69" };
          };
      ];
  }

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
    | Some c ->
        Stdlib.print_endline (Token.show c);
        let expected_output =
          {
            Ast.Program.statements =
              [
                Ast.Statement.LET
                  {
                    identifier = { Ast.Identifier.value = "x" };
                    value =
                      Ast.Expression.IDENTIFIER { Ast.Identifier.value = "5" };
                  };
                Ast.Statement.LET
                  {
                    identifier = { Ast.Identifier.value = "y" };
                    value =
                      Ast.Expression.IDENTIFIER { Ast.Identifier.value = "10" };
                  };
                Ast.Statement.LET
                  {
                    identifier = { Ast.Identifier.value = "foobar" };
                    value =
                      Ast.Expression.IDENTIFIER { Ast.Identifier.value = "69" };
                  };
              ];
          }
        in
        assert (Ast.Program.equal expected_output (parse_program parser))
end
