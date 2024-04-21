type exp = V of string | Abs of string * exp | App of exp * exp;;


type opcode = LOOKUP of string | APP | MKCLOS of string * opcode list | RET;;

let rec compile e = match e with 
V x -> [LOOKUP x]
| Abs (x, e1) -> MKCLOS(x, compile e1) :: [RET]
[MKCLOS (x, compile e1 @ [RET])] 
| App (e1, e2) -> compile(e1) @ compile(e2) @ [APP];;

