(* OCaml does not include a built-in regex library in its standard library,
   so this simplistic tokenizer will manually parse input strings. *)

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
 
 (* A simple function to classify a string as a token *)
 let classify_token str =
   match str with
   | "if" | "then" | "else" -> Keyword str
   | "true" -> Boolean true
   | "false" -> Boolean false
   | "+" | "-" | "*" | "/" -> ArithmeticOp str
   | "=" | "!=" | ">" | "<" | ">=" | "<=" -> ComparisonOp str
   | "(" | ")" -> Parenthesis str.[0]
   | "," -> Comma
   | _ when Str.string_match (Str.regexp "^[a-z_][a-zA-Z0-9']*$") str 0 -> Identifier str
   | _ when Str.string_match (Str.regexp "^[0-9]+$") str 0 -> Integer (int_of_string str)
   | _ -> failwith "Unrecognized token"
 
 (* This function is a placeholder to illustrate token classification.
    A real tokenizer would read input strings, segment them into substrings that
    represent potential tokens, and classify each substring using `classify_token` or similar logic. *)
 

