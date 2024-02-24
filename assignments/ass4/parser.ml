type token =
  | VARIABLE of (
# 18 "parser.mly"
        string
# 6 "parser.ml"
)
  | CONSTANT of (
# 18 "parser.mly"
        string
# 11 "parser.ml"
)
  | IDENTIFIER of (
# 18 "parser.mly"
        string
# 16 "parser.ml"
)
  | LPAREN
  | RPAREN
  | DOT
  | COMMA
  | COLON_MINUS
  | SEMICOLON

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Types
type program = clause list

and clause = Fact of predicate | Rule of predicate * body

and body = AtomicFormula of predicate
         | Sequence of body * body
         | Parallel of body * body

and predicate = Pred of string * term list

and term = Variable of string
         | Constant of string
         | Function of string * term list
# 42 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* DOT *);
  263 (* COMMA *);
  264 (* COLON_MINUS *);
  265 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* VARIABLE *);
  258 (* CONSTANT *);
  259 (* IDENTIFIER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\005\000\
\006\000\004\000\007\000\007\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\001\000\003\000\001\000\003\000\003\000\
\001\000\004\000\001\000\003\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\016\000\001\000\000\000\000\000\000\000\
\000\000\000\000\013\000\014\000\000\000\000\000\000\000\003\000\
\009\000\005\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\012\000\007\000\008\000\015\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\017\000\018\000\019\000\014\000\015\000"

let yysindex = "\010\000\
\009\255\000\000\010\255\000\000\000\000\007\255\008\255\000\255\
\009\255\009\255\000\000\000\000\011\255\012\255\013\255\000\000\
\000\000\000\000\253\254\000\255\000\000\000\255\009\255\009\255\
\014\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\255\000\000\
\018\000\000\000\000\000\000\000\000\000\000\000\017\255\000\000\
\000\000\000\000\018\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\014\000\000\000\255\255\242\255\000\000\241\255\000\000"

let yytablesize = 24
let yytable = "\007\000\
\011\000\012\000\013\000\023\000\025\000\024\000\026\000\007\000\
\027\000\028\000\001\000\003\000\009\000\008\000\020\000\010\000\
\021\000\002\000\029\000\022\000\004\000\011\000\016\000\006\000"

let yycheck = "\001\000\
\001\001\002\001\003\001\007\001\020\000\009\001\022\000\009\000\
\023\000\024\000\001\000\003\001\006\001\004\001\004\001\008\001\
\005\001\000\000\005\001\007\001\006\001\005\001\009\000\006\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  COMMA\000\
  COLON_MINUS\000\
  SEMICOLON\000\
  "

let yynames_block = "\
  VARIABLE\000\
  CONSTANT\000\
  IDENTIFIER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 26 "parser.mly"
                ( _1 )
# 123 "parser.ml"
               : program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'clause) in
    Obj.repr(
# 29 "parser.mly"
               ( [_1] )
# 130 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clause_list) in
    Obj.repr(
# 30 "parser.mly"
                           ( _1 :: _3 )
# 138 "parser.ml"
               : 'clause_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 33 "parser.mly"
              ( Fact _1 )
# 145 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 34 "parser.mly"
                               ( Rule (_1, _3) )
# 153 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 37 "parser.mly"
                   ( AtomicFormula _1 )
# 160 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 38 "parser.mly"
                              ( Sequence (_1, _3) )
# 168 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomic_formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'body) in
    Obj.repr(
# 39 "parser.mly"
                                  ( Parallel (_1, _3) )
# 176 "parser.ml"
               : 'body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 42 "parser.mly"
              ( _1 )
# 183 "parser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 45 "parser.mly"
                                       ( Pred (_1, _3) )
# 191 "parser.ml"
               : 'predicate))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 48 "parser.mly"
         ( [_1] )
# 198 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 49 "parser.mly"
                         ( _1 :: _3 )
# 206 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
             ( Variable _1 )
# 213 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
             ( Constant _1 )
# 220 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 54 "parser.mly"
                                       ( Function (_1, _3) )
# 228 "parser.ml"
               : 'term))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : program)
