type token =
  | ATOM of (
# 5 "parser.mly"
        string
# 6 "parser.mli"
)
  | VARIABLE of (
# 5 "parser.mly"
        string
# 11 "parser.mli"
)
  | DOT
  | LPAREN
  | RPAREN
  | COMMA
  | IMPLIES
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.program
