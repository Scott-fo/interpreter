let prompt = ">> "

let rec process_line lexer =
  let lexer, token = Lexer.next_token lexer in
  match token with
  | None -> ()
  | Some t ->
      Stdlib.print_endline (Token.show t);
      process_line lexer

let rec read () =
  Stdlib.print_string prompt;
  Stdlib.flush Stdlib.stdout;
  try
    let line = Stdlib.input_line Stdlib.stdin in
    let lexer = Lexer.init line in
    process_line lexer;
    read ()
  with End_of_file -> ()
