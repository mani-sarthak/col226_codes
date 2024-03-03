
type variable = string
type symbol = string
(* type signature = (symbol * int) list *)
type term = V of variable | Num of int | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
(* type substitution = (variable * term) list *)

val string_of_term : term -> string
val string_of_atom : atom -> string
val string_of_head : head -> string
val string_of_body : body -> string
val string_of_clause : clause -> string
val string_of_program : program -> string
val string_of_goal : goal -> string
(*val string_of_substitution : substitution -> string*)





(* 
type variable = string
type symbol = string
type signature = (symbol * int) list
type term = V of variable | Num of int | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list
 *)

(* type term = Variable of string | Atom of string
type predicate = string * term list
type clause = Fact of predicate | Rule of predicate * predicate list
type program = clause list 




val string_of_term : term -> string
val string_of_predicate : atom -> string
val string_of_clause : clause -> string
val string_of_program : program -> string
 *)
