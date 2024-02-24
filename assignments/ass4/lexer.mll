{
open Parser
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = lower (lower | upper | digit | '_')*
let variable = upper (lower | upper | digit | '_')*
let constant = lower (lower | upper | digit | '_')* | '"' [^ '"']* '"' | digit+

rule token = parse
  | [' ' '\t' '\n' '\r']+ { token lexbuf } (* Skip whitespace *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | ',' { COMMA }
  | ':' '-' { COLON_MINUS }
  | ';' { SEMICOLON }
  | variable as v { VARIABLE v }
  | constant as c { CONSTANT c }
  | ident as id { IDENTIFIER id }
  | eof { raise End_of_file }

{
exception Error
}
