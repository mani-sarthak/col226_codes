type variable = string
type symbol = string
type term = V of variable | Num of int | Wildcard | Node of symbol * (term list) | Tuple of term list 
type atom = A of symbol * (term list) | Not of atom
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list

let rec string_of_term = function
  | V v -> "VARIABLE " ^ v
  | Num n -> "NUMERAL " ^ string_of_int n
  | Node (s, ts) -> "NODE (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"
  | Wildcard -> "WILDCARD"
  | Tuple ts -> "TUPLE [" ^ String.concat "; " (List.map string_of_term ts) ^ "]"

let rec string_of_atom = function
  | A (s, ts) -> "ATOM (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"
  | Not a -> "NOT (" ^ string_of_atom a ^ ")"


let rec string_of_head = function
  | H a -> "HEAD (" ^ string_of_atom a ^ ")"

let rec string_of_body = function
  | B atoms -> "BODY [" ^ String.concat "; " (List.map string_of_atom atoms) ^ "]"

let rec string_of_clause = function
  | F h -> "FACT (" ^ string_of_head h ^ ")"
  | R (h, b) -> "RULE (" ^ string_of_head h ^ ", " ^ string_of_body b ^ ")"

let string_of_program program =
  String.concat "\n" (List.map string_of_clause program)

let string_of_goal (G atoms) =
  "GOAL [" ^ String.concat "; " (List.map string_of_atom atoms) ^ "]"
