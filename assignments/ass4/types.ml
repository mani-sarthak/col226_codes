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

let rec string_of_term = function
  | V v -> "V " ^ v
  | Num n -> "Num " ^ string_of_int n
  | Node (s, ts) -> "Node (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"

let rec string_of_atom (A (s, ts)) =
  "A (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"

let rec string_of_head = function
  | H a -> "H (" ^ string_of_atom a ^ ")"

let rec string_of_body = function
  | B atoms -> "B [" ^ String.concat "; " (List.map string_of_atom atoms) ^ "]"

let rec string_of_clause = function
  | F h -> "F (" ^ string_of_head h ^ ")"
  | R (h, b) -> "R (" ^ string_of_head h ^ ", " ^ string_of_body b ^ ")"

let string_of_program program =
  String.concat "\n" (List.map string_of_clause program)

let string_of_goal (G atoms) =
  "G [" ^ String.concat "; " (List.map string_of_atom atoms) ^ "]"
(* 

let rec string_of_substitution subs =
  String.concat "; " (List.map (fun (v, t) -> v ^ " -> " ^ string_of_term t) subs)


 *)



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








(* 

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
 
 
*)
