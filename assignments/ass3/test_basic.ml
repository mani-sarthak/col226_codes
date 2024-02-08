#load "str.cma"

(* Define the token type *)

type token =
  | Identifier of string
  | Integer of int
  | ArithmeticOp of char
  | Parenthesis of char

(* Exception for unknown tokens *)
exception Unknown_token of string

(* Function to tokenize a single word *)
let tokenize_word word =
  if Str.string_match (Str.regexp "^[a-z_][a-zA-Z0-9']*$") word 0 then
    Identifier word
  else if Str.string_match (Str.regexp "^[0-9]+$") word 0 then
    Integer (int_of_string word)
  else
    raise (Unknown_token word)

(* Function to tokenize a single character (for operators and parentheses) *)
let tokenize_char c =
  match c with
  | '+' | '-' | '*' | '/' -> ArithmeticOp c
  | '(' | ')' -> Parenthesis c
  | _ -> raise (Unknown_token (String.make 1 c))

(* Split input into tokens based on spaces and tokenize each part *)
let tokenize input =
  let words = Str.split (Str.regexp " +") input in
  List.map (fun word ->
    if String.length word = 1 then
      try tokenize_char word.[0] with
      | Unknown_token _ -> tokenize_word word
    else
      tokenize_word word
  ) words

(* Example usage *)
let () =
  let input = "x1 + 42 * ( y2 - 3 )" in
  let tokens = tokenize input in
  List.iter (fun token ->
    match token with
    | Identifier id -> Printf.printf "Identifier: %s\n" id
    | Integer n -> Printf.printf "Integer: %d\n" n
    | ArithmeticOp op -> Printf.printf "ArithmeticOp: %c\n" op
    | Parenthesis p -> Printf.printf "Parenthesis: %c\n" p
  ) tokens
