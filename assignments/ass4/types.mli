
type variable = string
type symbol = string
type term = V of variable | Num of int | Wildcard | Node of symbol * (term list) | Tuple of term list 
type atom = A of symbol * (term list) | Not of atom
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list

val string_of_term : term -> string
val string_of_atom : atom -> string
val string_of_head : head -> string
val string_of_body : body -> string
val string_of_clause : clause -> string
val string_of_program : program -> string
val string_of_goal : goal -> string