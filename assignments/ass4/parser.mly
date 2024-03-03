%{
open Types
%}

%token <string> VARIABLE ATOM
%token <int> NUM
%token <char> UNDEFINED
%token DOT SEMICOLON  LBRACKET RBRACKET LPAREN RPAREN COMMA PLUS MINUS TIMES DIV EQUAL GT LT OFC ENDL IMPLIES EOF
%start program
%type <Types.program> program

%%

program:
| clauses { $1 }

clauses:
| clause DOT clauses { $1 :: $3 }
| { [] }

clause:
| fact { Fact $1 }
| rule { $1 }

fact:
| ATOM LPAREN terms RPAREN { ($1, $3) }

rule:
| ATOM LPAREN terms RPAREN IMPLIES predicates { Rule(($1, $3), $6) }

terms:
| term COMMA terms { $1 :: $3 }
| term { [$1] }

term:
| VARIABLE { Variable $1 }
| ATOM { Atom $1 }

predicates:
| predicate COMMA predicates { $1 :: $3 }
| predicate { [$1] }

predicate:
| ATOM LPAREN terms RPAREN { ($1, $3) }
