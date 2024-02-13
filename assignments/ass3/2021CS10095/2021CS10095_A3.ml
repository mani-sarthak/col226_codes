(* Token types *)
type token =
  | Identifier of string
  | Keyword of string
  | Boolean of bool
  | BooleanOp of string
  | ArithmeticOp of string
  | Integer of int
  | ComparisonOp of string
  | StringLiteral of string
  | StringOp of string
  | Parenthesis of char
  | Comma
  | Semicolon 

(* Exception for unrecognized tokens *)
exception Unknown_token of string

(* Classify a string as a token *)
let classify_token str =
  match str with
  | "if" | "then" | "else" | "let" | "pair" | "fst" | "snd" | "and" | "in" | "match" | "with" | "function" | "rec"-> Keyword str
  | "true" -> Boolean true
  | "false" -> Boolean false
  | "+" | "-" | "*" | "/" -> ArithmeticOp str
  | "&&" | "||" | "!" -> BooleanOp str
  | "=" | "!=" | ">" | "<" | ">=" | "<=" -> ComparisonOp str
  | "^" -> StringOp str
  | "(" | ")" -> Parenthesis str.[0]
  | "," -> Comma
  | ";" -> Semicolon  
  | _ when Str.string_match (Str.regexp "^\".*\"$") str 0 ->
      StringLiteral (String.sub str 1 (String.length str - 2))
  | _ when Str.string_match (Str.regexp "^[1-9][0-9]*$") str 0 || Str.string_match (Str.regexp "^0$") str 0 ->
      Integer (int_of_string str)
  | _ when Str.string_match (Str.regexp "^[a-z_][a-zA-Z0-9'_]*$") str 0 ->
      Identifier str
  | _ -> raise (Unknown_token str)

(* Tokenize a string *)
let tokenize input =
  let rec aux acc i j =
    if j >= String.length input then
      if i < j then acc @ [classify_token (String.sub input i (j - i))]
      else acc
    else if input.[j] = '"' then 
      let end_idx = try String.index_from input (j + 1) '"' with
        | Not_found -> raise (Unknown_token "Unterminated string literal")
      in
      let str_literal = String.sub input j (end_idx - j + 1) in
      aux (acc @ [classify_token str_literal]) (end_idx + 1) (end_idx + 1)
    else match input.[j] with
      | ' ' | '\n' | '\t'  -> 
          if i < j then aux (acc @ [classify_token (String.sub input i (j - i))]) (j + 1) (j + 1)
          else aux acc (j + 1) (j + 1)
      | '+' | '-' | '*' | '/' | '=' | '!' | '>' | '<' | '(' | ')' | ',' | ';'-> 
          let acc = if i < j then acc @ [classify_token (String.sub input i (j - i))] else acc in
          aux (acc @ [classify_token (String.make 1 input.[j])]) (j + 1) (j + 1)
      | _ -> aux acc i (j + 1)
  in
  aux [] 0 0


(* Function to print tokens for testing *)
let print_token = function
  | Identifier id -> Printf.printf "Identifier(%s)\n" id
  | Keyword kw -> Printf.printf "Keyword(%s)\n" kw
  | Boolean b -> Printf.printf "Boolean(%b)\n" b
  | BooleanOp op -> Printf.printf "BooleanOp(%s)\n" op
  | ArithmeticOp op -> Printf.printf "ArithmeticOp(%s)\n" op
  | Integer n -> Printf.printf "Integer(%d)\n" n
  | ComparisonOp op -> Printf.printf "ComparisonOp(%s)\n" op
  | StringLiteral s -> Printf.printf "StringLiteral(%s)\n" s
  | StringOp op -> Printf.printf "StringOp(%s)\n" op
  | Parenthesis p -> Printf.printf "Parenthesis(%c)\n" p
  | Comma -> Printf.printf "Comma\n"
  | Semicolon -> Printf.printf "Semicolon\n"

(* Example test cases *)
let test_cases = [
  "if x1 + 42 * (y2 - 3) = 36 then \"result\" else ; x1";
  "pair(fst(x), snd(y)) && !false || true";
  "x' + y_2 - 10 >= 20";
  "\"Hello,world!\"";
  "1 + 1";
  "0";
  "let x = \"hello\" ^ \"world\" ;;";
  "if x1 + 49842 * (y2'__ - 3) = true then \"result\" else x1, let x2 = \"hello\" = \"world\"  && false ";
  "x1 + 42 * (y'2'_ - 3 ) = (true && false) || pair";
  "\"He says \"\"Hello\"\" to Him\" + 123";
  "let x = 123 let y = ; \"hello\" ^ \" hello\" ; ";
]

(* Tokenize and print tokens for each test case *)
let () =
  List.iter (fun test_case ->
    Printf.printf "Test case: %s\n" test_case;
    let tokens = tokenize test_case in
    List.iter print_token tokens;
    print_endline "\n\n\n\n";
  ) test_cases
