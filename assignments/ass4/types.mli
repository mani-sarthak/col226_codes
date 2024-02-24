type term = Variable of string | Atom of string
type predicate = string * term list
type clause = Fact of predicate | Rule of predicate * predicate list
type program = clause list


