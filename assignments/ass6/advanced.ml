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
| Bool of bool
| Not of exp
| And of exp * exp
| Or of exp * exp
| Implies of exp * exp
| Eq of exp * exp
| Neq of exp * exp
| Lt of exp * exp
| Gt of exp * exp
| Le of exp * exp
| Ge of exp * exp
| IfTE of exp * exp * exp
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
| BOOL of bool
| NOT
| AND
| OR
| IMPLIES
| EQ
| NEQ
| LT
| GT
| LE
| GE
| COND of (opcode list) * (opcode list)
;;

type table = (string * answer) list
and answer = Vclos of table * string * control | Int of int | Bool of bool
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
| Bool(b) -> [BOOL(b)]
| Not(e1) -> (compile e1) @ [NOT]
| And(e1, e2) -> (compile e1) @ (compile e2) @ [AND]
| Or(e1, e2) -> (compile e1) @ (compile e2) @ [OR]
| Implies(e1, e2) -> (compile e1) @ (compile e2) @ [IMPLIES]
| Eq(e1, e2) -> (compile e1) @ (compile e2) @ [EQ]
| Neq(e1, e2) -> (compile e1) @ (compile e2) @ [NEQ]
| Lt(e1, e2) -> (compile e1) @ (compile e2) @ [LT]
| Gt(e1, e2) -> (compile e1) @ (compile e2) @ [GT]
| Le(e1, e2) -> (compile e1) @ (compile e2) @ [LE]
| Ge(e1, e2) -> (compile e1) @ (compile e2) @ [GE]
| IfTE(e1, e2, e3) -> (compile e1) @ [COND((compile e2), (compile e3))]
;;




let rec secd = function
| (x::s, e, [], _) -> x
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
| (s, e, BOOL(b)::c, d) -> secd(Bool(b)::s, e, c, d)
| (Bool(b)::s, e, NOT::c, d) -> secd(Bool(not b)::s, e, c, d)
| (Bool(b2)::Bool(b1)::s, e, AND::c, d) -> secd(Bool(b1 && b2)::s, e, c, d)
| (Bool(b2)::Bool(b1)::s, e, OR::c, d) -> secd(Bool(b1 || b2)::s, e, c, d)
| (Bool(b2)::Bool(b1)::s, e, IMPLIES::c, d) -> secd(Bool((not b1) || b2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, EQ::c, d) -> secd(Bool(n1 = n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, NEQ::c, d) -> secd(Bool(n1 <> n2)::s, e, c, d)
| (Bool(b2)::Bool(b1)::s, e, EQ::c, d) -> secd(Bool(b1 = b2)::s, e, c, d)
| (Bool(b2)::Bool(b1)::s, e, NEQ::c, d) -> secd(Bool(b1 <> b2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, LT::c, d) -> secd(Bool(n1 < n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, GT::c, d) -> secd(Bool(n1 > n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, LE::c, d) -> secd(Bool(n1 <= n2)::s, e, c, d)
| (Int(n2)::Int(n1)::s, e, GE::c, d) -> secd(Bool(n1 >= n2)::s, e, c, d)
| (Bool(b)::s, e, COND(c1, c2)::c, d) -> if b then secd(s, e, c1 @ c, d) else secd(s, e, c2 @ c, d)
| _ -> raise InvalidOperation
;;


let _env = [];;
let exec c = secd([], _env, c, []);;

(* Testcases *)

let e1 = Abs("x", Var("x"));;
let e2 = Abs("x", Abs("y", App(Var("x"), Var("y"))));;
let e3 = Abs("x", Abs("y", App(Var("y"), Int(3))));;
let e4 = App(Abs("x", Add(Var "x", Int 7)), Int 3);; 
let e5 = And(Bool(true), Bool(false));;
let e6 = Le(Int(3), Int(4));;
let e7 = IfTE(Le(Int(3), Int(4)), Int(3), Int(4));;


let c1 = compile e1;;
let c2 = compile e2;;
let c3 = compile e3;;
let c4 = compile e4;;
let c5 = compile e5;;
let c6 = compile e6;;
let c7 = compile e7;;


exec c1;;
secd([], [("x", Int(3))], c1, []);;
exec c2;;
exec c3;;
exec c4;;
exec c5;;
exec c6;;
exec c7;;

let exp1 : exp = Var("y");;                           (* y *)
let _env = [("y", Int(4))];;                          (* y = 4 *)
let exp2 : exp = Abs("x", Add(Var "x", Int 7));;      (* \x. (x + 7) *)
let exp3 : exp = Pow(exp1, Int(2));;                  (* y^2 = 16 *)
let exp4 : exp = Gt(Int(3), Int(4));;                 (* 3 > 4 = false *)
let expn : exp = IfTE(exp4, exp1, App(exp2, exp3));;  (* exp4 = false => APP( x + 7 where x is 16*)
secd ([], _env, compile expn, []);;                   (* returns 23 as answer *)
