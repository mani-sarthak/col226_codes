(* main.ml *)
open Types
let parse_program lexbuf =
  try
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
    Printf.eprintf "At offset %d: syntax error.\n" (Lexing.lexeme_start lexbuf);
    exit (-1)

let () =
  let input_channel = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = parse_program lexbuf in
  (* Here you can process the AST or print it, for example *)
  print_endline "Parsing successful!"
