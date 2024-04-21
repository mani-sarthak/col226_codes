type myBool = T | F;;

let myBool2bool b = match b with
  T -> true
  | F -> false
;;


type exp = Num of int | Bl of myBool
| V of string (* variables *)
| Plus of exp * exp | Times of exp * exp
| And of exp * exp | Or of exp * exp | Not of exp
| Eq of exp * exp | Gt of exp * exp
| IfTE of exp * exp * exp
| Pair of exp * exp
| Fst of exp | Snd of exp
(* | Abs of string * exp | App of exp * exp *)
;;
let test1 = Plus (Times (Num 3, Num 4), Times (Num 5, Num 6));;
let test2 = Or (Not (Bl T), And (Bl T, Or(Bl F, Bl T)));;
let test3 = Gt (Times (Num 5, Num 6), (Times (Num 3, Num 4)));;
let test4 = And (Eq(test1, Num 42), Not test3);;

let rec ht e = match e with
Num n -> 0
| Bl b -> 0
| V x -> 0
| Plus (e1, e2) -> 1 + (max (ht e1) (ht e2))
| Times (e1, e2) -> 1 + (max (ht e1) (ht e2))
| And (e1, e2) -> 1 + (max (ht e1) (ht e2))
| Or (e1, e2) -> 1 + (max (ht e1) (ht e2))
| Not e1 -> 1 + (ht e1)
| Eq (e1, e2) -> 1 + (max (ht e1) (ht e2))
| Gt(e1, e2) -> 1 + (max (ht e1) (ht e2))
| IfTE(e0, e1, e2) -> 1 + (max (ht e0) (max (ht e1) (ht e2)))
| Pair(e1, e2) -> 1 + (max (ht e1) (ht e2))
| Fst(e0) -> 1 + (ht e0)
| Snd(e0) -> 1 + (ht e0)
;;

let h1 = ht test1;;
let h2 = ht test2;;
let h3 = ht test3;;
let h4 = ht test4;;


let rec size e = match e with
Num n -> 1
| Bl b -> 1
| V x -> 1
| Plus (e1, e2) -> 1 + (size e1) + (size e2)
| Times (e1, e2) -> 1 + (size e1) + (size e2)
| And (e1, e2) -> 1 + (size e1) + (size e2)
| Or (e1, e2) -> 1 + (size e1) + (size e2)
| Not e1 -> 1 + (size e1)
| Eq (e1, e2) -> 1 + (size e1) + (size e2)
| Gt(e1, e2) -> 1 + (size e1) + (size e2)
| IfTE(e0, e1, e2) -> 1 + (size e0) + (size e1) + (size e2)
| Pair(e1, e2) -> 1 + (size e1) + (size e2)
| Fst(e0) -> 1 + (size e0)
| Snd(e0) -> 1 + (size e0)
;;

let s1 = size test1;;
let s2 = size test2;;
let s3 = size test3;;
let s4 = size test4;;



(* this have partial match  *)
type values = N of int | B of bool | P of values * values ;;
let rec eval e rho = match e with
Num n -> N n
| Bl b -> B (myBool2bool b)
| V x -> rho x
| Plus (e1, e2) -> let N n1 = (eval e1 rho)
and N n2 = (eval e2 rho)
in N (n1 + n2)
| Times (e1, e2) -> let N n1 = (eval e1 rho)
and N n2 = (eval e2 rho)
in N (n1 * n2)
| And (e1, e2) -> let B b1 = (eval e1 rho)
and B b2 = (eval e2 rho)
in B (b1 && b2)
| Or (e1, e2) -> let B b1 = (eval e1 rho)
and B b2 = (eval e2 rho)
in B (b1 || b2)
| Not e1 -> let B b1 = (eval e1 rho) in B (not b1)
| Eq (e1, e2) -> let N n1 = (eval e1 rho)
and N n2 = (eval e2 rho)
in B (n1 = n2)
| Gt(e1, e2) -> let N n1 = (eval e1 rho)
and N n2 = (eval e2 rho)
in B (n1 > n2)
| IfTE(e0, e1, e2) -> let B b0 = (eval e0 rho)
in if b0 then (eval e1 rho)
else (eval e2 rho)
| Pair(e1, e2) -> let v1 = (eval e1 rho)
and v2 = (eval e2 rho)
in P(v1, v2)
| Fst(e0) -> let P(v1,v2) = (eval e0 rho)
in v1
| Snd(e0) -> let P(v1,v2) = (eval e0 rho)
in v2
;;


let v1 = eval test1;;
let v2 = eval test2;;
let v3 = eval test3;;
let v4 = eval test4;;


type opcode = LDN of int | LDB of bool | LOOKUP of string
| PLUS | TIMES | AND | OR | NOT | EQ | GT
| COND of opcode list * opcode list
| PAIR | FST | SND
;;


let rec compile e = match e with
Num n -> [ LDN n ]
| Bl b -> [LDB (myBool2bool b) ] (* Constants *)
| V x -> [LOOKUP x] (* Variables *)
| Plus (e1, e2) -> (compile e1) @ (compile e2) @ [PLUS]
| Times (e1, e2) -> (compile e1) @ (compile e2) @ [TIMES]
| And (e1, e2) -> (compile e1) @ (compile e2) @ [AND]
| Or (e1, e2) -> (compile e1) @ (compile e2) @ [OR]
| Not e1 -> (compile e1) @ [NOT]
| Eq (e1, e2) -> (compile e1) @ (compile e2) @ [EQ]
| Gt(e1, e2) -> (compile e1) @ (compile e2) @ [GT]
| IfTE(e0, e1, e2) -> (compile e0) @ [COND(compile e1, compile e2)]
| Pair(e1, e2) -> (compile e1) @ (compile e2) @ [PAIR]
| Fst(e0) -> (compile e0) @ [FST]
| Snd(e0) -> (compile e0) @ [SND]
;;

let c1 = compile test1;;
let c2 = compile test2;;
let c3 = compile test3;;
let c4 = compile test4;;


exception Stuck of (string -> values) * values list * opcode
list;;


let rec stkmc g s c = match s, c with
v::_, [ ] -> v (* no more opcodes, return top *)
| s, (LDN n)::c' -> stkmc g ((N n)::s) c'
| s, (LDB b)::c' -> stkmc g ((B b)::s) c'
| s, (LOOKUP x)::c' -> stkmc g ((g x)::s) c'
| (N n2)::(N n1)::s', PLUS::c' -> stkmc g (N(n1+n2)::s') c'
| (N n2)::(N n1)::s', TIMES::c' -> stkmc g (N(n1*n2)::s') c'
| (B b2)::(B b1)::s', AND::c' -> stkmc g(B(b1 && b2)::s') c'
| (B b2)::(B b1)::s', OR::c' -> stkmc g (B(b1 || b2)::s') c'
| (B b1)::s', NOT::c' -> stkmc g (B(not b1)::s') c'
| (N n2)::(N n1)::s', EQ::c' -> stkmc g (B(n1 = n2)::s') c'
| (N n2)::(N n1)::s', GT::c' -> stkmc g (B(n1 > n2)::s') c'
| (B true)::s', COND(c1,c2)::c' -> stkmc g s' (c1 @ c')
| (B false)::s', COND(c1,c2)::c' -> stkmc g s' (c2 @ c')
| s, PAIR::c' -> let v2::v1::s' = s in stkmc g (P(v1,v2)::s') c'
| (P(v1,v2))::s', FST::c' -> stkmc g (v1::s') c'
| (P(v1,v2))::s', SND::c' -> stkmc g (v2::s') c'
| _, _ -> raise (Stuck (g, s, c))
;;


let gamma1  x= match x with 
"x" -> N 2
| "" -> B true;;
let w1 = stkmc gamma1 [ ] (c1) ;;
let w2 = stkmc gamma1 [ ] (c2) ;;
let w3 = stkmc gamma1 [ ] (c3) ;;
let w4 = stkmc gamma1 [ ] (c4) ;;


