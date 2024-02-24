let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let result = Parser.program Lexer.token lexbuf in
    print_endline (Types.string_of_program result);
    print_endline "Parsed successfully."
  with
  | Lexer.Error msg ->
    Printf.fprintf stderr "Lexer error: %s\n" msg
  | Parsing.Parse_error ->
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.fprintf stderr "Parser error at line %d, position %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
