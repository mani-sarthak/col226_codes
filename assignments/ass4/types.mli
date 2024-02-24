type term = Variable of string | Atom of string
type predicate = string * term list
type clause = Fact of predicate | Rule of predicate * predicate list
type program = clause list


val string_of_term : term -> string
val string_of_predicate : predicate -> string
val string_of_clause : clause -> string
val string_of_program : program -> string
