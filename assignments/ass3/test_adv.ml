(* Define the token types *)
type token =
  | Identifier of string
  | Keyword of string
  | Boolean of bool
  | ArithmeticOp of string
  | Integer of int
  | ComparisonOp of string
  | StringLiteral of string
  | Parenthesis of char
  | Comma

(* Exception for unrecognized tokens *)
exception Unknown_token of string

(* Manually classify a string as a token *)
let classify_token str =
  let is_integer s =
    try ignore (int_of_string s); true
    with Failure _ -> false
  in
  match str with
  | "if" | "then" | "else" | "let" -> Keyword str
  | "true" -> Boolean true
  | "false" -> Boolean false
  | "+" | "-" | "*" | "/" -> ArithmeticOp str
  | "=" | "!=" | ">" | "<" | ">=" | "<=" -> ComparisonOp str
  | "(" | ")" -> Parenthesis str.[0]
  | "," -> Comma
  | _ when str.[0] = '"' && str.[String.length str - 1] = '"' ->
      StringLiteral (String.sub str 1 (String.length str - 2))
  | _ when is_integer str -> Integer (int_of_string str)
  | _ -> Identifier str  (* Identifier as a fallback *)

(* Tokenize an input string *)
let tokenize input =
  let rec aux acc i j =
    if j >= String.length input then
      if i < j then acc @ [classify_token (String.sub input i (j - i))]
      else acc
    else match input.[j] with
      | ' ' | '\n' | '\t' -> 
          if i < j then aux (acc @ [classify_token (String.sub input i (j - i))]) (j + 1) (j + 1)
          else aux acc (j + 1) (j + 1)
      | '+' | '-' | '*' | '/' | '=' | '!' | '>' | '<' | '(' | ')' | ',' -> 
          let acc = if i < j then acc @ [classify_token (String.sub input i (j - i))] else acc in
          aux (acc @ [classify_token (String.make 1 input.[j])]) (j + 1) (j + 1)
      | _ -> aux acc i (j + 1)
  in
  aux [] 0 0

(* Example usage *)
let () =
  let input = "if x1 + 42 * (y2 - 3) = true then \"result\" else x1, let x2 = \"mani\" " in
  (* let input = "x1 + 42 * (y'2'_ - 3 ) = (true & false)" in *)
  let tokens = tokenize input in
  List.iter (fun token ->
    match token with
    | Identifier id -> Printf.printf "Identifier: %s\n" id
    | Keyword kw -> Printf.printf "Keyword: %s\n" kw
    | Boolean b -> Printf.printf "Boolean: %b\n" b
    | ArithmeticOp op -> Printf.printf "ArithmeticOp: %s\n" op
    | Integer n -> Printf.printf "Integer: %d\n" n
    | ComparisonOp op -> Printf.printf "ComparisonOp: %s\n" op
    | StringLiteral s -> Printf.printf "StringLiteral: \"%s\"\n" s
    | Parenthesis p -> Printf.printf "Parenthesis: %c\n" p
    | Comma -> Printf.printf "Comma\n"
  ) tokens
