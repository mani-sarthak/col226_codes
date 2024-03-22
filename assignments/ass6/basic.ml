type exp = Var of string
| Abs of string * exp
| App of exp * exp
| Int of int
| Absolute of exp
| Add of exp * exp
| Sub of exp * exp
| Mul of exp * exp
| Div of exp * exp
| Mod of exp * exp
| Pow of exp * exp
;; 

type opcode = LOOKUP of string
| CLOS of string * (opcode list)
| RET
| APP
| INT of int
| ABSOLUTE
| ADD
| SUB
| MUL
| DIV
| MOD
| POW
;;

type table = (string * answer) list
and answer = Vclos of table * string * control | Int of int
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
| Int(n) -> [INT(n)]
| Absolute(e1) -> (compile e1) @ [ABSOLUTE]
| Add(e1, e2) -> (compile e1) @ (compile e2) @ [ADD]
| Sub(e1, e2) -> (compile e1) @ (compile e2) @ [SUB]
| Mul(e1, e2) -> (compile e1) @ (compile e2) @ [MUL]
| Div(e1, e2) -> (compile e1) @ (compile e2) @ [DIV]
| Mod(e1, e2) -> (compile e1) @ (compile e2) @ [MOD]
| Pow(e1, e2) -> (compile e1) @ (compile e2) @ [POW]
;;


let rec secd = function
| (s, e, LOOKUP(x)::c, d) -> secd((lookup x e)::s, e, c, d)
| (s, e, CLOS(x, c1)::c, d) -> secd((Vclos(e, x, c1))::s, e, c, d)
| (x::Vclos(e1, x1, c1)::s, e, APP::c, d) -> secd([], (x1, x)::e1, c1, (s, e, c)::d)
| (x::s, e, RET::c, (s1, e1, c1)::d) -> secd(x::s1, e1, c1, d)
| (s, e, INT(n)::c, d) -> secd(Int(n)::s, e, c, d)
| (Int(n)::s, e, ABSOLUTE::c, d) -> secd(Int(if n > 0 then n else ((-1)*n))::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, ADD::c, d) -> secd(Int(n1 + n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, SUB::c, d) -> secd(Int(n1 - n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, MUL::c, d) -> secd(Int(n1 * n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, DIV::c, d) -> secd(Int(n1 / n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, MOD::c, d) -> secd(Int(n1 mod n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, POW::c, d) -> secd(Int(int_of_float ((float_of_int n1) ** (float_of_int n2)))::s, e, c, d)
| _ -> raise InvalidOperation
;;


(* Examples *)

let e1 = Abs("x", Var("x"));;
let e2 = Abs("x", Abs("y", App(Var("x"), Var("y"))));;
let e3 = Abs("x", Abs("y", App(Var("y"), Int(3))));;
let e4 = App(Abs("x", Add(Var "x", Int 7)), Int 3);; 

let c1 = compile e1;;
let c2 = compile e2;;
let c3 = compile e3;;
let c4 = compile e4;;