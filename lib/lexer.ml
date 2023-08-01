open Base

type t = { input : string; position : int; ch : char option }

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let read_char lexer =
  if lexer.position >= String.length lexer.input - 1 then
    { lexer with ch = None }
  else
    let position = lexer.position + 1 in
    let ch = Some (String.get lexer.input position) in
    { lexer with position; ch }

let next_token lexer =
  Option.value_map lexer.ch ~default:(lexer, None) ~f:(fun ch ->
      let open Token in
      let token =
        match ch with
        | '=' -> ASSIGN
        | '+' -> PLUS
        | ',' -> COMMA
        | ';' -> SEMICOLON
        | '(' -> LPAREN
        | ')' -> RPAREN
        | '{' -> LBRACE
        | '}' -> RBRACE
        | _ -> failwith "Not yet implemented"
      in
      (read_char lexer, Some token))

module Test = struct
  let%test_unit "next token, simple" =
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
    let lexer = init input in
    let _ =
      List.fold ~init:lexer
        ~f:(fun lexer expected ->
          let lexer', output = next_token lexer in
          assert (Poly.equal output (Some expected));
          lexer')
        expected_output
    in
    ()
end
