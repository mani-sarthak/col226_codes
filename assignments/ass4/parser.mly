%{
open Types
%}

%token <string> VARIABLE ATOM
%token <int> NUM
%token <char> UNDEFINED
%token DOT SEMICOLON LBRACKET RBRACKET LPAREN RPAREN COMMA PLUS MINUS TIMES DIV EQUAL NEQ GT LT PIPE OFC IMPLIES UNDERSCORE GEQ LEQ NOT EOF
%start program goal
%type <Types.program> program
%type <Types.goal> goal


%left SEMICOLON COMMA 
%left PLUS MINUS 
%left TIMES DIV 
%nonassoc EQ LT GT DOT


%%

program:
    EOF                                 {[]}
  | clauses EOF                         {$1}
;

clauses:
    clause                              {[$1]}
  | clause clauses                  {($1)::$2}
;

clause:
    atom DOT                           {F(H($1))} 
  | atom IMPLIES disjunction DOT      {R(H($1), B($3))}
;
disjunction:
    conjunction                             { $1 } 
  | conjunction SEMICOLON disjunction    { $1 @ $3 }
;
conjunction:
    atom                                { [$1] } 
  | atom COMMA conjunction                { $1 :: $3 }
;
goal:
    conjunction DOT                      {G($1)}
;


atom:
  | ATOM                                {A($1, [])}
  | ATOM LPAREN terms RPAREN               {A($1, $3)}
  | term EQUAL term                        {A("_eq", [$1; $3])}
  | term NEQ term                    {A("_not_eq", [$1; $3])}
  | term LT term                        {A("<", [$1; $3])}
  | term GT term                        {A(">", [$1; $3])}
  | term LEQ term                    {A("<=", [$1; $3])}
  | term GEQ term                    {A(">=", [$1; $3])}
  | OFC                                 {A("_ofc", [])}
  | NOT atom                           { Not($2) }
;

terms:
    term                                {[$1]}
  | term COMMA terms                {($1)::$3}
;

term:
    LPAREN tuple RPAREN                          { Tuple($2) }
  | VARIABLE                                 {V($1)}
  | ATOM                                {Node($1, [])}
  | NUM                                 {Num($1)}
  | ATOM LPAREN terms RPAREN                {Node($1, $3)}
  | term PLUS term                      {Node("+", [$1; $3])}
  | term MINUS term                     {Node("-", [$1; $3])}
  | term TIMES term                      {Node("*", [$1; $3])}
  | term DIV term                       {Node("/", [$1; $3])}
  | list                                {$1}
  | UNDERSCORE                          {Wildcard}
;

tuple:
    term COMMA term                     { [$1; $3] }
  | term COMMA tuple                    { $1 :: $3 }

list:
    LBRACKET RBRACKET                               {Node("_empty_list", [])}
  | LBRACKET list_element RBRACKET                     {$2}
;

list_element:
    term                                 {Node("_list", [$1; Node("_empty_list", [])])}
  | term COMMA list_element                 {Node("_list", [$1; $3])}
  | term PIPE term                       {Node("_list", [$1; $3])}
  | term PIPE UNDERSCORE               {Node("_list", [$1; Wildcard])}  
;
