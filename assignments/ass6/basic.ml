type exp = Var of string
| Abs of string * exp
| App of exp * exp
;; 

type opcode = LOOKUP of string
| CLOS of string * (opcode list)
| RET
| APP
;;

type table = (string * answer) list
and answer = Vclos of table * string * control
and stack = answer list
and environment = table
and control = opcode list
and dump = (stack * environment * control) list
;;

exception InvalidOperation;;
exception Variable_not_intialized;;


let rec lookup x env = match env with
| [] -> raise Variable_not_intialized
| (y, v)::tl -> if x = y then v else lookup x tl
;;


let rec compile e = match e with
| Var(x) -> [LOOKUP(x)]
| Abs(x, e1) -> [CLOS(x, (compile e1) @ [RET])]
| App(e1, e2) -> (compile e1) @ (compile e2) @ [APP]
;;


let rec secd = function
| (s, e, LOOKUP(x)::c, d) -> secd((lookup x e)::s, e, c, d)
| (s, e, CLOS(x, c1)::c, d) -> secd((Vclos(e, x, c1))::s, e, c, d)
| (x::Vclos(e1, x1, c1)::s, e, APP::c, d) -> secd([], (x1, x)::e1, c1, (s, e, c)::d)
| (x::s, e, RET::c, (s1, e1, c1)::d) -> secd(x::s1, e1, c1, d)
| _ -> raise InvalidOperation
;;


(* Examples *)

let e1 = Abs("x", Var("x"));;
let e2 = Abs("x", Abs("y", App(Var("x"), Var("y"))));;

let c1 = compile e1;;
let c2 = compile e2;;
