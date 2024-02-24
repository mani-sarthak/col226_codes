open Printf

let read_input () =
  let rec aux acc =
    try
      let line = input_line stdin in
      aux (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  aux ""

let print_tokens lexbuf =
  let out_channel = open_out "tokens.txt" in 
  try
    while true do
      let token = Lexer.token lexbuf in
      match token with
      | EOF -> raise Exit 
      | _ ->
        let token_str =
        match token with
        | ATOM at -> sprintf "ATOM(%s)" at
        | VARIABLE var -> sprintf "VARIABLE(%s)" var
        | DOT -> sprintf "DOT" 
        | LPAREN -> sprintf "LPAREN" 
        | RPAREN -> sprintf "RPAREN" 
        | COMMA -> sprintf "COMMA"    
        | IMPLIES -> sprintf "IMPLIES" 
        | _ -> "OtherToken"
        in
        Printf.fprintf out_channel "%s\n" token_str
    done
  with Exit -> close_out out_channel

let process_input input =
  let lexbuf_tokens = Lexing.from_string input in
  let lexbuf_parse = Lexing.from_string input in
  
  print_tokens lexbuf_tokens;
  
  try
    let result = Parser.program Lexer.token lexbuf_parse in
    print_endline (Types.string_of_program result);
    print_endline "Parsed successfully."
  with
  | Lexer.Error msg ->
    Printf.fprintf stderr "Lexer error: %s\n" msg
  | Parsing.Parse_error ->
    let pos = lexbuf_parse.Lexing.lex_curr_p in
    Printf.fprintf stderr "Parser error at line %d, position %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let () =
  let input = read_input () in
  process_input input

