{
  open Parser
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let variable = ['A'-'Z'](alpha_num*)
let constant = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"")
let separator = [' ' '\t' '\n' '\r']+
let number = '0'|['1'-'9']['0'-'9']*

rule read = parse
  | separator     { read lexbuf }
  | variable as v { VARIABLE(v) }
  | constant as c { ATOM(c) } 
  | number as n   { NUM(int_of_string n) }
  | '.' { DOT }
  | ';' { SEMICOLON }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '=' { EQUAL }
  | '>' { GT }
  | '<' { LT }
  | '!' { OFC }
  | '.' { ENDL }
  | ":-" { IMPLIES }
  | '%'  { single_line_comment lexbuf }
  | "/*" { multi_line_comment 0 lexbuf }
  | _ as x { UNDEFINED(x) }
  | eof { EOF }


and single_line_comment = parse
    eof                   {EOF}
  | '\n'                  {read lexbuf}
  |   _                   {single_line_comment lexbuf}

and multi_line_comment depth = parse
    eof                   {failwith "Syntax error: End of file in /* ... */ comment"}
  | "*/"                  {if depth = 0 then read lexbuf else multi_line_comment (depth-1) lexbuf}
  | "/*"                  {multi_line_comment (depth+1) lexbuf}
  |  _                    {multi_line_comment depth lexbuf}
