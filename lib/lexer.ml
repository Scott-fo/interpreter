type t = { input : string; position : int }

let init input = { input; position = 0 }
let next_token lexer = failwith "NYI"

module Test = struct
  let%test "next token" =
    let input = "=+(){},;" in
    let expected_output =
      [
        Token.ASSIGN;
        Token.PLUS;
        Token.LPAREN;
        Token.RPAREN;
        Token.LBRACE;
        Token.RBRACE;
        Token.COMMA;
        Token.SEMICOLON;
      ]
    in
    let lexer = Lexer.init input in
    List.for_all
      (fun expected ->
        let output = Lexer.next_token lexer in
        output = Some expected)
      expected_output
end
