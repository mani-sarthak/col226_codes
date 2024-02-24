type term = Variable of string | Atom of string
type predicate = string * term list
type clause = Fact of predicate | Rule of predicate * predicate list
type program = clause list


let rec string_of_term = function
  | Variable v -> "Variable " ^ v
  | Atom a -> "Atom " ^ a

let rec string_of_predicate (name, terms) =
  name ^ "(" ^ String.concat ", " (List.map string_of_term terms) ^ ")"

let rec string_of_clause = function
  | Fact p -> "Fact (" ^ string_of_predicate p ^ ")"
  | Rule (p, ps) -> "Rule (" ^ string_of_predicate p ^ ", [" ^ String.concat "; " (List.map string_of_predicate ps) ^ "])"

let string_of_program program =
  String.concat "\n" (List.map string_of_clause program)
