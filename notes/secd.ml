type value =
  | Int of int
  | Closure of string * code * env
and opcode = 
  | LOOKUP of string
  | APP
  | MKCLOS of string * code
  | RET
and code = opcode list
and env = (string * value) list
and stack = value list
and dump = (stack * env * code) list


type expr =
  | V of string
  | Abs of string * expr
  | App of expr * expr


let rec compile e = match e with 
  V x -> [LOOKUP x]
  | Abs (x, e1) ->  [MKCLOS (x, (compile e1) @ [RET])] 
  | App (e1, e2) -> compile(e1) @ compile(e2) @ [APP];;

  

let rec eval s e c d =
  match c with
  | [] -> (match s with
           | [v] -> v 
           | _ -> failwith "Invalid final stack state")
  | instr :: c' -> match instr with
      | LOOKUP x ->
          let a = List.assoc x e in eval (a :: s) e c' d
      | APP ->
          (match s with
           |  arg :: Closure(param, body, closure_env) :: s' ->
               eval [] ((param, arg) :: closure_env) body ((s', e, c') :: d)
           | _ -> failwith "APP: Stack does not contain a closure and an argument")
      | MKCLOS(x, body) ->
          let closure = Closure(x, body, e) in
          eval (closure :: s) e c' d
      | RET ->
          (match d with
           | (s', e', c') :: d' -> eval (List.hd s :: s') e' c' d'
           | [] -> failwith "RET: Dump is empty")

let run_secd code =
  eval [] [("y", Int(1024)); ("x", Int(3))] code []

(* Example usage *)
let example_exp = App(Abs("x", V "x"), V "y") (* should return the value of y given in the environment *)
let compiled_code = compile example_exp
let result = run_secd compiled_code

(* let () =
  match result with
  | Int n -> print_endline (string_of_int n)
  | Closure _ -> print_endline "Result is a closure" *)
