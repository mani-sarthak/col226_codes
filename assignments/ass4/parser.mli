type token =
  | VARIABLE of (
# 18 "parser.mly"
        string
# 6 "parser.mli"
)
  | CONSTANT of (
# 18 "parser.mly"
        string
# 11 "parser.mli"
)
  | IDENTIFIER of (
# 18 "parser.mly"
        string
# 16 "parser.mli"
)
  | LPAREN
  | RPAREN
  | DOT
  | COMMA
  | COLON_MINUS
  | SEMICOLON

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> program
