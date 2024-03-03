%{
open Types
%}

%token <string> VARIABLE ATOM
%token <int> NUM
%token <char> UNDEFINED
%token DOT SEMICOLON LBRACKET RBRACKET LPAREN RPAREN COMMA PLUS MINUS TIMES DIV EQUAL NEQ GT LT PIPE OFC IMPLIES EOF
%start program
%type <Types.program> program


%left COMMA PLUS MINUS TIMES DIV SEMICOLON
%nonassoc EQ LT GT DOT


%%

program:
    EOF                                 {[]}
  | clauses EOF                     {$1}
;

clauses:
    clause                              {[$1]}
  | clause clauses                  {($1)::$2}
;

clause:
    atom DOT                           {F(H($1))}
  | atom IMPLIES atoms DOT            {R(H($1), B($3))}
;

goal:
    atoms DOT                      {G($1)}
;

atoms:
    atom                                {[$1]}
  | atom COMMA atoms                {($1)::$3}
;

atom:
    /* LP atom RP                          {$2} */
  | ATOM                                {A($1, [])}
  | ATOM LPAREN terms RPAREN               {A($1, $3)}
  | term EQUAL term                        {A("_eq", [$1; $3])}
  | term NEQ term                    {A("_not_eq", [$1; $3])}
  | term LT term                        {A("<", [$1; $3])}
  | term GT term                        {A(">", [$1; $3])}
  | OFC                                 {A("_ofc", [])}
;

terms:
    term                                {[$1]}
  | term COMMA terms                {($1)::$3}
;

term:
    LPAREN term RPAREN                          {$2}
  | VARIABLE                                 {V($1)}
  | ATOM                                {Node($1, [])}
  | NUM                                 {Num($1)}
  | ATOM LPAREN terms RPAREN                {Node($1, $3)}
  | term PLUS term                      {Node("+", [$1; $3])}
  | term MINUS term                     {Node("-", [$1; $3])}
  | term TIMES term                      {Node("*", [$1; $3])}
  | term DIV term                       {Node("/", [$1; $3])}
  | list                                {$1}
;

list:
    LBRACKET RBRACKET                               {Node("_empty_list", [])}
  | LBRACKET list_body RBRACKET                     {$2}
;

list_body:
    term                                 {Node("_list", [$1; Node("_empty_list", [])])}
  | term COMMA list_body                 {Node("_list", [$1; $3])}
  | term PIPE term                       {Node("_list", [$1; $3])}
;























// program:
// | clauses { $1 }

// clauses:
// | clause DOT clauses { $1 :: $3 }
// | { [] }

// clause:
// | fact { Fact $1 }
// | rule { $1 }

// fact:
// | ATOM LPAREN terms RPAREN { ($1, $3) }

// rule:
// | ATOM LPAREN terms RPAREN IMPLIES predicates { Rule(($1, $3), $6) }

// terms:
// | term COMMA terms { $1 :: $3 }
// | term { [$1] }

// term:
// | VARIABLE { Variable $1 }
// | ATOM { Atom $1 }

// predicates:
// | predicate COMMA predicates { $1 :: $3 }
// | predicate { [$1] }

// predicate:
// | ATOM LPAREN terms RPAREN { ($1, $3) }
