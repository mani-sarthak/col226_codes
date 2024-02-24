%{
    open Types
%}

%{
type program = clause list

and clause = Fact of predicate | Rule of predicate * body

and body = AtomicFormula of predicate
         | Sequence of body * body
         | Parallel of body * body

and predicate = Pred of string * term list

and term = Variable of string
         | Constant of string
         | Function of string * term list
%}

%token <string> VARIABLE CONSTANT IDENTIFIER
%token LPAREN RPAREN DOT COMMA COLON_MINUS SEMICOLON
%start program
%type <program> program

%%

program:
  | clause_list { $1 }

clause_list:
  | clause DOT { [$1] }
  | clause DOT clause_list { $1 :: $3 }

clause:
  | predicate { Fact $1 }
  | predicate COLON_MINUS body { Rule ($1, $3) }

body:
  | atomic_formula { AtomicFormula $1 }
  | atomic_formula COMMA body { Sequence ($1, $3) }
  | atomic_formula SEMICOLON body { Parallel ($1, $3) }

atomic_formula:
  | predicate { $1 }

predicate:
  | IDENTIFIER LPAREN term_list RPAREN { Pred ($1, $3) }

term_list:
  | term { [$1] }
  | term COMMA term_list { $1 :: $3 }

term:
  | VARIABLE { Variable $1 }
  | CONSTANT { Constant $1 }
  | IDENTIFIER LPAREN term_list RPAREN { Function ($1, $3) }
