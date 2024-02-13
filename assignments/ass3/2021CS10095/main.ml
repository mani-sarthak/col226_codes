let () =
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  Your_lexer.scanner input_file output_file
