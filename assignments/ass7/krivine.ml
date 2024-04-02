type exp = Var of string 
| Abs of string * exp
| App of exp * exp
| Int of int
| Add of exp * exp
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

let add (c1, c2) = match (c1, c2) with
  (CL (Int i1, env1), CL (Int i2, env2)) -> CL (Int (i1+i2), [])
  | _ -> raise InvalidOperation
;;

let rec krivine cl s = match cl with
| CL (Var x, env) -> krivine (lookup (Var(x), env)) s
| CL (Abs (x, e), env) -> 
    let (cl', s') = absApp (cl, s) in
    krivine cl' s'
| CL (App (e1, e2), env) -> krivine (CL (e1, env)) (CL (e2, env)::s)
| CL (Int i, env) -> CL (Int i, env)
| CL (Add (e1, e2), env) -> krivine (add ((krivine (CL (e1, env)) []), (krivine (CL (e2, env)) []))) s 
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
