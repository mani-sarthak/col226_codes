(* Extend the token type definition to include boolean operators *)
type token =
  | Identifier of string
  | Keyword of string
  | Boolean of bool
  | BooleanOp of string  (* Add this line for boolean operators *)
  | ArithmeticOp of string
  | Integer of int
  | ComparisonOp of string
  | StringLiteral of string
  | Parenthesis of char
  | Comma

(* Update the classify_token function to recognize boolean operators *)
let classify_token str =
  match str with
  | "if" | "then" | "else" -> Keyword str
  | "true" -> Boolean true
  | "false" -> Boolean false
  | "&&" | "||" | "!" -> BooleanOp str  (* Add this line to match boolean operators *)
  | "+" | "-" | "*" | "/" -> ArithmeticOp str
  | "=" | "!=" | ">" | "<" | ">=" | "<=" -> ComparisonOp str
  | "(" | ")" -> Parenthesis str.[0]
  | "," -> Comma
  | _ when Str.string_match (Str.regexp "^\".*\"$") str 0 ->
      StringLiteral (String.sub str 1 (String.length str - 2))
  | _ when Str.string_match (Str.regexp "^[1-9][0-9]*$") str 0 || Str.string_match (Str.regexp "^0$") str 0 ->
      Integer (int_of_string str)
  | _ when Str.string_match (Str.regexp "^[a-z_][a-zA-Z0-9']*$") str 0 ->
      Identifier str
  | _ -> raise (Unknown_token str)

(* The rest of the tokenizer remains the same *)
