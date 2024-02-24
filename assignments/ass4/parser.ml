type token =
  | ATOM of (
# 5 "parser.mly"
        string
# 6 "parser.ml"
)
  | VARIABLE of (
# 5 "parser.mly"
        string
# 11 "parser.ml"
)
  | DOT
  | LPAREN
  | RPAREN
  | COMMA
  | IMPLIES
  | EOF

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Types
# 24 "parser.ml"
let yytransl_const = [|
  259 (* DOT *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* COMMA *);
  263 (* IMPLIES *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ATOM *);
  258 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\005\000\006\000\
\006\000\008\000\008\000\007\000\007\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\000\000\001\000\001\000\004\000\006\000\003\000\
\001\000\001\000\001\000\003\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\001\000\000\000\004\000\005\000\
\000\000\000\000\011\000\010\000\000\000\000\000\002\000\000\000\
\000\000\000\000\008\000\000\000\007\000\000\000\000\000\000\000\
\000\000\012\000\014\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\007\000\008\000\013\000\021\000\014\000\
\022\000"

let yysindex = "\002\000\
\003\255\000\000\001\255\000\000\000\000\004\255\000\000\000\000\
\000\255\003\255\000\000\000\000\005\255\002\255\000\000\006\255\
\000\255\008\255\000\000\007\255\000\000\009\255\000\255\008\255\
\011\255\000\000\000\000"

let yyrindex = "\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\012\255\000\000\015\255\
\000\000\000\000\000\000\000\000\000\000\016\255\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\004\000\000\000\000\000\000\000\239\255\252\255\000\000\
\000\000"

let yytablesize = 20
let yytable = "\019\000\
\011\000\012\000\001\000\003\000\009\000\025\000\010\000\017\000\
\020\000\016\000\023\000\003\000\018\000\015\000\024\000\027\000\
\009\000\006\000\013\000\026\000"

let yycheck = "\017\000\
\001\001\002\001\001\000\001\001\004\001\023\000\003\001\006\001\
\001\001\005\001\004\001\000\000\007\001\010\000\006\001\005\001\
\005\001\003\001\003\001\024\000"

let yynames_const = "\
  DOT\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  IMPLIES\000\
  EOF\000\
  "

let yynames_block = "\
  ATOM\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'clauses) in
    Obj.repr(
# 13 "parser.mly"
          ( _1 )
# 105 "parser.ml"
               : Types.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'clauses) in
    Obj.repr(
# 16 "parser.mly"
                     ( _1 :: _3 )
# 113 "parser.ml"
               : 'clauses))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
  ( [] )
# 119 "parser.ml"
               : 'clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 20 "parser.mly"
       ( Fact _1 )
# 126 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 21 "parser.mly"
       ( _1 )
# 133 "parser.ml"
               : 'clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'terms) in
    Obj.repr(
# 24 "parser.mly"
                           ( (_1, _3) )
# 141 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'terms) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'predicates) in
    Obj.repr(
# 27 "parser.mly"
                                              ( Rule((_1, _3), _6) )
# 150 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'terms) in
    Obj.repr(
# 30 "parser.mly"
                   ( _1 :: _3 )
# 158 "parser.ml"
               : 'terms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 31 "parser.mly"
       ( [_1] )
# 165 "parser.ml"
               : 'terms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
           ( Variable _1 )
# 172 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
       ( Atom _1 )
# 179 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'predicate) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'predicates) in
    Obj.repr(
# 38 "parser.mly"
                             ( _1 :: _3 )
# 187 "parser.ml"
               : 'predicates))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'predicate) in
    Obj.repr(
# 39 "parser.mly"
            ( [_1] )
# 194 "parser.ml"
               : 'predicates))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'terms) in
    Obj.repr(
# 42 "parser.mly"
                           ( (_1, _3) )
# 202 "parser.ml"
               : 'predicate))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.program)
