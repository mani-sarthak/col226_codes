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
(* | Pair of exp * exp
| Fst of exp
| Snd of exp *)
;;

type answer = INT of int | BOOL of bool | PAIR of answer * answer
and closure = CL of exp * environmentCLOS
and stackCLOS = closure list
and environmentCLOS = (exp * closure) list
;;

exception Uninitialised_Var;;
exception InvalidOperation;;
exception InvalidClosure;;
exception EmptyProgram;;


let rec lookup (x, env) = match (x, env) with
  | (x, (e,cl)::c') -> if e<>x then lookup (x, c') else
          (match cl with
          | CL (Abs (x1,e'), env) -> CL (Abs (x1, e'), (e,cl)::env)
          | _ -> cl)
  | (x, []) -> raise Uninitialised_Var
;;

let absApp (cl, s) = match (cl, s) with
	| (CL (Abs(x ,e), env), c::c') -> (CL (e, (Var x, c)::env), c')
	| (_,[]) -> raise InvalidOperation
	| _ -> raise InvalidOperation
;;

let absolute cl = match cl with
  | CL (Int n, env) -> CL (Int(if n > 0 then n else ((-1)*n)), [])
  | _ -> raise InvalidOperation
;;

let add (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1+i2), [])
  | _ -> raise InvalidOperation
;;

let sub (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1-i2), [])
  | _ -> raise InvalidOperation
;;

let mul (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1*i2), [])
  | _ -> raise InvalidOperation
;;

let div (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1/i2), [])
  | _ -> raise InvalidOperation
;;

let modulus (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1 mod i2), [])
  | _ -> raise InvalidOperation
;;

let pow (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (int_of_float (float_of_int i1 ** float_of_int i2)), [])
  | _ -> raise InvalidOperation
;;

let not_op cl = match cl with
  CL (Bool b, env) -> CL (Bool (not b), [])
  | _ -> raise InvalidOperation

let and_op (cl1, cl2) = match (cl1, cl2) with
  (CL (Bool b1, env1), CL (Bool b2, env2)) -> CL (Bool (b1 && b2), [])
  | _ -> raise InvalidOperation
;;

let or_op (cl1, cl2) = match (cl1, cl2) with
  (CL (Bool b1, env1), CL (Bool b2, env2)) -> CL (Bool (b1 || b2), [])
  | _ -> raise InvalidOperation
;;

let implies (cl1, cl2) = match (cl1, cl2) with
  (CL (Bool b1, env1), CL (Bool b2, env2)) -> CL (Bool ((not b1) || b2), [])
  | _ -> raise InvalidOperation
;;

let eq (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 = i2), [])
| (CL (Bool b1, env1), CL (Bool b2, env2)) -> CL (Bool (b1 = b2), [])
| _ -> raise InvalidOperation
;;

let neq (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 <> i2), [])
| (CL (Bool b1, env1), CL (Bool b2, env2)) -> CL (Bool (b1 <> b2), [])
| _ -> raise InvalidOperation
;;

let lt (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 < i2), [])
| _ -> raise InvalidOperation
;;

let gt (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 > i2), [])
| _ -> raise InvalidOperation
;;

let le (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 <= i2), [])
| _ -> raise InvalidOperation
;;

let ge (cl1, cl2) = match (cl1, cl2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Bool (i1 >= i2), [])
| _ -> raise InvalidOperation
;;

let rec krivine cl s = match cl with
| CL (Var x, env) -> krivine (lookup (Var(x), env)) s
| CL (Abs (x, e), env) -> 
    let (cl', s') = absApp (cl, s) in
    krivine cl' s'
| CL (App (e1, e2), env) -> krivine (CL (e1, env)) (CL (e2, env)::s)
| CL (Int i, env) -> CL (Int i, env)
| CL (Absolute e, env) -> krivine (absolute (krivine (CL (e, env)) [])) s
| CL (Add (e1, e2), env) -> krivine (add ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s 
| CL (Sub (e1, e2), env) -> krivine (sub ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Mul (e1, e2), env) -> krivine (mul ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Div (e1, e2), env) -> krivine (div ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Mod (e1, e2), env) -> krivine (modulus ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Pow (e1, e2), env) -> krivine (pow ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Bool b, env) -> CL (Bool b, env)
| CL (Not e, env) -> krivine (not_op (krivine (CL (e, env)) [])) s
| CL (And (e1, e2), env) -> krivine (and_op ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Or (e1, e2), env) -> krivine (or_op ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Implies (e1, e2), env) -> krivine (implies ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Eq (e1, e2), env) -> krivine (eq ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Neq (e1, e2), env) -> krivine (neq ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Lt (e1, e2), env) -> krivine (lt ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Gt (e1, e2), env) -> krivine (gt ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Le (e1, e2), env) -> krivine (le ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (Ge (e1, e2), env) -> krivine (ge ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s
| CL (IfTE (e1, e2, e3), env) -> 
    (match krivine (CL (e1, env)) [] with
    | CL (Bool b, _) -> if b then (krivine (CL (e2, env)) s) else (krivine (CL (e3, env)) s)
    | _ -> raise InvalidOperation
    )
;;

let rec exec prog env = match prog with
| p::prog' ->
  let cl = krivine (CL (p, env)) [] in
  (match cl with
  | CL (Int i, _) -> INT i
  | CL (Bool b, _) -> BOOL b
  (* | CL (Pair (e1, e2), _) -> PAIR (exec [e1] env, exec [e2] env) *)
  | _ -> raise InvalidClosure
  )
| _ -> raise EmptyProgram
;;


(* Testcases *)
let e1 = App(Abs("x", Var("x")), Int(7));;
exec [e1] [];;
let e2 = App(Abs("x", Add(Var("x"), Int(13))), Int(7));;
exec [e2] [];;
let e3 = Add(Int(7), Int(13));;
exec [e3] [];;
let e4 = Add(Int(7), Var("x"));;
exec [e4] [(Var("x"), CL(Int(30), []))];;
let e5 = Absolute(Int(-7));;
exec [e5] [];;
let e6 = Sub(Int(7), Int(13));;
exec [e6] [];;
let e7 = Absolute(Mul(App(Abs("x", Add(Var "x", Int(7))), Int(-3)), e6));;
exec [e7] [];;
let e8 = Mod(e7, Int(11));;
exec [e8] [];;
let e9 = Sub(e7, e8);;
exec [e9] [];;
let e10 = Div(e9, Int(11));;
exec [e10] [];;
let e11 = Pow(e10, Int(2));;
exec [e11] [];;



let e12 = Bool(true);;
exec [e12] [];;
let e13 = Not(Bool(true));;
exec [e13] [];;
let e14 = And(Bool(true), e13);;
exec [e14] [];;
let e15 = Or(Bool(true), e13);;
exec [e15] [];;
let e16 = Implies(Bool(true), e13);;
exec [e16] [];;
let e17 = Implies(Bool(false), e13);;
exec [e17] [];;
let e18 = Implies(e17, e15);;
exec [e18] [];;


let e19 = Eq(Int(7), Int(7));;
exec [e19] [];;
let e20 = Neq(Int(7), Int(-7));;
exec [e20] [];;
let e21 = Lt(Int(7), Int(7));;
exec [e21] [];;
let e22 = Gt(Int(8), Int(7));;
exec [e22] [];;
let e23 = Le(Int(7), Int(7));;
exec [e23] [];;
let e24 = Ge(Int(8), Int(7));;
exec [e24] [];;


let e25 = IfTE(Bool(false), Int(7), Int(13));;
exec [e25] [];;


(* let e26 = Pair(Int(7), Int(13));;
exec [e26] [];;
let e27 = Fst(e26);;
exec [e27] [];;
let e28 = Snd(e26);;
exec [e28] [];; *)