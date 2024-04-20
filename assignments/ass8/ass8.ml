(* As specified in the assignment *)

type variable = string;;
type predicate = string;;
type constant = string;;
type term =
    | V    of variable
    | C    of constant
    | A of predicate * (term list);;
type atomic_formula = A of predicate * (term list);;
type head = atomic_formula;;
type body = atomic_formula list;;
type clause =
    | Fact of head
    | Rule of head * body;;
type goal = atomic_formula list;;
type program = clause list;;

