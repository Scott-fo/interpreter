open Base

type t = { input : string; position : int; ch : char option }

let init input =
  if String.is_empty input then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }

let is_letter = function
  | None -> false
  | Some ch -> Char.is_alpha ch || Char.equal ch '_'

let is_int = function None -> false | Some i -> Char.is_digit i
let is_whitespace = function None -> false | Some ch -> Char.is_whitespace ch

let rec next_token lexer =
  let open Token in
  let lexer = skip_whitespace lexer in
  Option.value_map lexer.ch ~default:(lexer, None) ~f:(fun ch ->
      let lexer, token =
        match ch with
        | '=' -> check_peeked lexer '=' ~eq:EQ ~ineq:ASSIGN
        | '!' -> check_peeked lexer '=' ~eq:NOT_EQ ~ineq:BANG
        | '+' -> (read_char lexer, PLUS)
        | '-' -> (read_char lexer, MINUS)
        | '*' -> (read_char lexer, ASTERISK)
        | '/' -> (read_char lexer, SLASH)
        | '<' -> (read_char lexer, LT)
        | '>' -> (read_char lexer, GT)
        | ',' -> (read_char lexer, COMMA)
        | ';' -> (read_char lexer, SEMICOLON)
        | '(' -> (read_char lexer, LPAREN)
        | ')' -> (read_char lexer, RPAREN)
        | '{' -> (read_char lexer, LBRACE)
        | '}' -> (read_char lexer, RBRACE)
        | ch when is_letter (Some ch) -> read_ident lexer
        | ch when is_int (Some ch) -> read_int lexer
        | c ->
            failwith
              (Printf.sprintf "Unexpected token. Got string value: %s"
                 (Char.to_string c))
      in
      (lexer, Some token))

and read_char lexer =
  let length = String.length lexer.input in
  if lexer.position >= length - 1 then
    { lexer with position = length; ch = None }
  else
    let position = lexer.position + 1 in
    let ch = Some (String.get lexer.input position) in
    { lexer with position; ch }

and walk lexer ~rule =
  let rec loop lexer =
    if rule lexer.ch then loop (read_char lexer) else lexer
  in
  let lexer' = loop lexer in
  (lexer', lexer'.position)

and read_while lexer ~rule =
  let start_pos = lexer.position in
  let lexer, end_pos = walk lexer ~rule in
  (lexer, String.sub lexer.input ~pos:start_pos ~len:(end_pos - start_pos))

and read_ident lexer =
  let lexer, ident = read_while lexer ~rule:is_letter in
  (lexer, Token.lookup_ident ident)

and read_int lexer =
  let lexer, num = read_while lexer ~rule:is_int in
  (lexer, Token.INT num)

and skip_whitespace lexer =
  let lexer, _ = walk lexer ~rule:is_whitespace in
  lexer

and peek_char lexer =
  if lexer.position >= String.length lexer.input - 1 then None
  else Some (String.get lexer.input (lexer.position + 1))

and check_peeked lexer ch ~eq ~ineq =
  let lexer', token =
    match peek_char lexer with
    | Some peeked when Char.equal ch peeked -> (read_char lexer, eq)
    | _ -> (lexer, ineq)
  in
  (read_char lexer', token)

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
          let output' =
            match output with
            | Some token -> token
            | None -> failwith "Unexpected end of tokens"
          in
          assert (Token.equal output' expected);
          lexer')
        expected_output
    in
    ()

  let%test_unit "next token, simple let" =
    let input = "let" in
    let expected_output = [ Token.LET ] in
    let lexer = init input in
    let _ =
      List.fold ~init:lexer
        ~f:(fun lexer expected ->
          let lexer', output = next_token lexer in
          let output' =
            match output with
            | Some token -> token
            | None -> failwith "Unexpected end of tokens"
          in
          let error_message =
            Printf.sprintf "Expected: %s, Got: %s" (Token.show expected)
              (Token.show output')
          in
          assert (
            if not (Token.equal output' expected) then failwith error_message
            else true);
          lexer')
        expected_output
    in
    ()

  let%test_unit "next token, simple let with semicolon" =
    let input = "let;" in
    let expected_output = [ Token.LET; Token.SEMICOLON ] in
    let lexer = init input in
    let _ =
      List.fold ~init:lexer
        ~f:(fun lexer expected ->
          let lexer', output = next_token lexer in
          let output' =
            match output with
            | Some token -> token
            | None -> failwith "Unexpected end of tokens"
          in
          let error_message =
            Printf.sprintf "Expected: %s, Got: %s" (Token.show expected)
              (Token.show output')
          in
          assert (
            if not (Token.equal output' expected) then failwith error_message
            else true);
          lexer')
        expected_output
    in
    ()

  let%test_unit "next token, 22" =
    let input = "22" in
    let expected_output = [ Token.INT "22" ] in
    let lexer = init input in
    let _ =
      List.fold ~init:lexer
        ~f:(fun lexer expected ->
          let lexer', output = next_token lexer in
          let output' =
            match output with
            | Some token -> token
            | None -> failwith "Unexpected end of tokens"
          in
          let error_message =
            Printf.sprintf "Expected: %s, Got: %s" (Token.show expected)
              (Token.show output')
          in
          assert (
            if not (Token.equal output' expected) then failwith error_message
            else true);
          lexer')
        expected_output
    in
    ()

  let%test_unit "next token, complex" =
    let input =
      {|
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5
        5 < 10 > 5

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        |}
    in
    let expected_output =
      [
        Token.LET;
        Token.IDENT "five";
        Token.ASSIGN;
        Token.INT "5";
        Token.SEMICOLON;
        Token.LET;
        Token.IDENT "ten";
        Token.ASSIGN;
        Token.INT "10";
        Token.SEMICOLON;
        Token.LET;
        Token.IDENT "add";
        Token.ASSIGN;
        Token.FUNCTION;
        Token.LPAREN;
        Token.IDENT "x";
        Token.COMMA;
        Token.IDENT "y";
        Token.RPAREN;
        Token.LBRACE;
        Token.IDENT "x";
        Token.PLUS;
        Token.IDENT "y";
        Token.SEMICOLON;
        Token.RBRACE;
        Token.SEMICOLON;
        Token.LET;
        Token.IDENT "result";
        Token.ASSIGN;
        Token.IDENT "add";
        Token.LPAREN;
        Token.IDENT "five";
        Token.COMMA;
        Token.IDENT "ten";
        Token.RPAREN;
        Token.SEMICOLON;
        Token.BANG;
        Token.MINUS;
        Token.SLASH;
        Token.ASTERISK;
        Token.INT "5";
        Token.INT "5";
        Token.LT;
        Token.INT "10";
        Token.GT;
        Token.INT "5";
        Token.IF;
        Token.LPAREN;
        Token.INT "5";
        Token.LT;
        Token.INT "10";
        Token.RPAREN;
        Token.LBRACE;
        Token.RETURN;
        Token.TRUE;
        Token.SEMICOLON;
        Token.RBRACE;
        Token.ELSE;
        Token.LBRACE;
        Token.RETURN;
        Token.FALSE;
        Token.SEMICOLON;
        Token.RBRACE;
        Token.INT "10";
        Token.EQ;
        Token.INT "10";
        Token.SEMICOLON;
        Token.INT "10";
        Token.NOT_EQ;
        Token.INT "9";
        Token.SEMICOLON;
      ]
    in
    let lexer = init input in
    let _ =
      List.fold ~init:lexer
        ~f:(fun lexer expected ->
          let lexer', output = next_token lexer in
          let output' =
            match output with
            | Some token -> token
            | None -> failwith "Unexpected end of tokens"
          in
          let error_message =
            Printf.sprintf "Expected: %s, Got: %s" (Token.show expected)
              (Token.show output')
          in
          assert (
            if not (Token.equal output' expected) then failwith error_message
            else true);
          lexer')
        expected_output
    in
    ()
end
