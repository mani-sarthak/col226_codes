open Printf

let print_tokens lexbuf out_channel =
  let rec aux () =
    let token = Lexer.token lexbuf in
    match token with
    | EOF -> ()
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
        (* | EOF  -> sprintf "EOF(_)" *)
        | _ -> "OtherToken"
      in
      fprintf out_channel "%s\n" token_str;  (* Write to the specified output channel *)
      aux ()
  in
  aux ()

let () =
  let action, out_channel =
    if Array.length Sys.argv > 1 then
      match Sys.argv.(1) with
      | "-print-tokens" -> `PrintTokens, Some (open_out Sys.argv.(2))
      | _ -> `Parse, None
    else `Parse, None
  in

  let lexbuf = Lexing.from_channel stdin in

  match action with
  | `PrintTokens ->
    begin
      match out_channel with
      | Some oc ->
        print_tokens lexbuf oc;
        close_out oc
      | None -> failwith "Output file not specified"
    end
  | `Parse ->
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


(* 
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
    Printf.fprintf stderr "Parser error at line %d, position %d\n" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) *)
