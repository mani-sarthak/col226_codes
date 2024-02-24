{
  open Parser
  exception Error of string
  let raise_error msg = raise (Error msg)
}

rule token = parse
  | [' ' '\t' '\r' '\n']+ { token lexbuf }
  | '.' { DOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | ":-" { IMPLIES }
  | [ 'a'-'z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { ATOM(lxm) }
  | [ 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { VARIABLE(lxm) }
  | _ as char { raise_error (Printf.sprintf "Unexpected character: %c" char) }
  | eof { EOF }
