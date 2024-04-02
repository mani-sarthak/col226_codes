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

type answer = Int of int
and stack = answer list
and program = exp list
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
;;

let rec exec prog env = match prog with
| p::prog' ->
  let cl = krivine (CL (p, env)) [] in
  (match cl with
  | CL (Int i, _) -> Int i
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

