type variable = string
type symbol = string
type term = V of variable | Num of int | Wildcard | Node of symbol * (term list) | Tuple of term list 
type atom = A of symbol * (term list) 
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list


exception InvalidProgram
exception NOT_UNIFIABLE
exception NotPossible
exception NotFound

let rec foldl f e l = match l with
    [] -> e
  | x::xs -> foldl f (f e x) xs
;;

let rec checkProgram prog = match prog with
  | [] -> true
  | F (H (A (s, ts)))::tl | R (H (A (s, ts)), _)::tl ->
      (match s with
        | "_eq_" | "_not_eq" | "<" | ">" | ">=" | "<=" -> raise InvalidProgram
        | _ -> checkProgram tl)
;;



let rec map f list = match list with
    [] -> []
  | x::xs -> (f x)::map f xs
;;

let rec modifyTerm i t = match t with
  V (v) -> V ((string_of_int i) ^  v)
  | Node (s, ts) -> Node (s, map (modifyTerm i) ts)
  | Tuple ts -> Tuple (map (modifyTerm i) ts)
  | _ -> t
;;


let rec modifyAtom i a = match a with
  A(s, ts) -> A(s, map (modifyTerm i) ts)
;;

let rec modifyClause cl i = match cl with
  F (H (a)) -> F (H (modifyAtom i a))
  | R (H (a), B (atoms)) -> R (H (modifyAtom i a), B (map (modifyAtom i) atoms))
;;


let rec modifyInitialProg prog i = match prog with
    [] -> []
  | cl::ps -> (modifyClause cl i)::modifyInitialProg ps (i+1)
;;


let rec string_of_term = function
  | V v -> "VARIABLE " ^ v
  | Num n -> "NUMERAL " ^ string_of_int n
  | Node (s, ts) -> "NODE (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"
  | Wildcard -> "WILDCARD"
  | Tuple ts -> "TUPLE [" ^ String.concat "; " (List.map string_of_term ts) ^ "]"

let rec string_of_atom = function
  | A (s, ts) -> "ATOM (" ^ s ^ ", [" ^ String.concat "; " (List.map string_of_term ts) ^ "])"
  (* | Not a -> "NOT (" ^ string_of_atom a ^ ")" *)


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

;;


let rec exists x y = match y with
    [] -> false
  | z::ys -> (x = z) || (exists x ys)
;;


let rec union l1 l2 = match l1 with
    [] -> l2
  | x::xs -> if (exists x l2) then union xs l2
             else x::(union xs l2)
;;

let rec print_term_list tl = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ",";
      print_term_list tls;
    )

and print_list_body t = match t with
    Node("_empty_list", []) -> Printf.printf ""
  | Node("_list", [t1; Node("_empty_list", [])]) -> print_term t1
  | Node("_list", [t1; t2]) -> (
      print_term t1;
      Printf.printf ",";
      print_list_body t2;
    )
  | _ -> raise NotPossible

and print_term t = match t with
    V(v) -> Printf.printf " %s " v
  | Node("_empty_list", []) -> Printf.printf " [] "
  | Node(s, []) -> Printf.printf " %s " s
  | Node("_list", _) -> (
      Printf.printf " [";
      print_list_body t;
      Printf.printf "] ";
    )
  | Node(s, l) -> (
      Printf.printf " %s ( " s;
      print_term_list l;
      Printf.printf " ) ";
    )
  | Num(n) -> Printf.printf " %d " n
;;


let rec printAns unifier = match unifier with
    [] -> Printf.printf "true. "
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printAns xs;
    )
;;


let rec getAns unif vars = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NotFound
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getAns unif vs
      with NotFound -> getAns unif vs
;;


let rec modifyProg2 (prog:program) (A(s, _): atom): program = match prog with
    [] -> []
  | cl::ps -> match cl with F(H(A(s', _))) | R(H(A(s', _)), _) ->
                if s = s' then (modifyClause cl 0)::modifyProg2 ps (A(s, []))
                else cl::modifyProg2 ps (A(s, []))
;;


let rec variableInTerm v t =
  match t with
      V(x) -> x = v
    | Node(s, l) -> foldl (||) false (map (variableInTerm v) l)
    | _ -> false
;;


let rec subst s t =
  match t with
      Node(s', l) -> Node(s', map (subst s) l)
    | Num(_) -> t
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

let compose s1 s2 =
  let f s x = (fst x, subst s (snd x)) in (map (f s2) s1) @ s2
;;

let rec combine l1 l2 = match l1 with
    [] -> []
  | x::xs -> (x, (List.hd l2))::combine xs (List.tl l2)
;;



let rec mgu_term t1 t2 =
  match (t1, t2) with
      (V(x), V(y)) -> if x = y then []
                      else [(x, V(y))]
    | (V(x), Node(_, _)) -> if variableInTerm x t2 then raise NOT_UNIFIABLE
                            else [(x, t2)]
    | (Node(_, _), V(y)) -> if variableInTerm y t1 then raise NOT_UNIFIABLE
                            else [(y, t1)]
    | (Num(n1), Num(n2)) -> if n1 = n2 then [] else raise NOT_UNIFIABLE
    | (Num(n1), V(x)) -> [(x, t1)]
    | (V(x), Num(n2)) -> [(x, t2)] 
    | (Node(s1, l1), Node(s2, l2)) ->
        if s1 <> s2 || (List.length l1 <> List.length l2) then raise NOT_UNIFIABLE
        else
          let f s tt = compose s (mgu_term (subst s (fst tt)) (subst s (snd tt))) in
          foldl f [] (combine l1 l2)
    | _ -> raise NOT_UNIFIABLE
;;

let mgu_atom (A(s1, l1)) (A(s2, l2)) = mgu_term (Node(s1, l1)) (Node(s2, l2));;


let rec subst s t =
  match t with
      Node(s', l) -> Node(s', map (subst s) l)
    | Num(_) -> t
    | V(x) -> match s with
                  [] -> t
                | s'::xs -> if fst s' = x then snd s' else subst xs t
;;

let rec subst_atom (s) (A(s', l)) = A(s', map (subst s) l)
;;



let solve_atomScam a1 a2 unif =
  compose unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))
;;


let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res
;;

let rec simplify_term t = match t with
    Num(_) -> t
  | Node("+", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 + n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("-", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 - n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("*", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 * n2)
        | _ -> raise NOT_UNIFIABLE
    )
  | Node("/", [t1; t2]) -> (
      match ((simplify_term t1), (simplify_term t2)) with
          (Num(n1), Num(n2)) -> Num(n1 / n2)
        | _ -> raise NOT_UNIFIABLE
      )
  | _ -> t
;;



let eval a unif = match a with
    A("_eq", [t1; t2])
  | A("_not_eq", [t1; t2]) -> compose unif (mgu_term (simplify_term (subst unif t1)) (simplify_term (subst unif t2)))
  | A(">", [t1; t2]) -> (
        match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
            (Num(n1), Num(n2)) -> if n1 > n2 then unif else raise NOT_UNIFIABLE
          | _ -> raise NOT_UNIFIABLE
    )
  | A("<", [t1; t2]) -> (
      match (simplify_term (subst unif t1), simplify_term (subst unif t2)) with
          (Num(n1), Num(n2)) -> if n1 < n2 then unif else raise NOT_UNIFIABLE
        | _ -> raise NOT_UNIFIABLE
    )
  | _ -> unif
;;



let rec solve_goal prog g unif vars = match g with 
G([]) -> (
  printAns (getAns unif vars);
  flush stdout;
  let choice = ref (get1char ()) in
  while(!choice <> '.' && !choice <> ';') do
    Printf.printf "\nUnknown Action: %c \nAction? " (!choice);
    flush stdout;
    choice := get1char();
  done;
  Printf.printf "\n";
  if !choice = '.' then (true, [])
  else (false, [])
)
| G(a::gs) -> match a with 
    A("_eq", _) | A(">", _) | A("<", _) | A("<=", _) | A(">=", _)-> (
      try solve_goal prog (G(gs)) (eval a unif) vars
      with NOT_UNIFIABLE -> (false, [])
    )
| A("_not_eq", _) -> (
  try (false, eval a unif)
  with NOT_UNIFIABLE -> solve_goal prog (G(gs)) unif vars
)
| A("_ofc", _) -> let _ = solve_goal prog (G(gs)) unif vars in (true, [])
| _ -> let new_prog = modifyProg2 prog a in
          let rec iter prog' = match prog' with
              [] -> (false, [])
            | cl::ps -> match cl with
                F(H(a')) -> (
                  try
                    let u = (solve_atomScam a' a unif) in
                    match (solve_goal new_prog (G(gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
              | R(H(a'), B(al)) -> (
                  try
                    let u = (solve_atomScam a' a unif) in
                    match (solve_goal new_prog (G(al @ gs)) u vars) with
                        (true, u') -> (true, u')
                      | _ -> iter ps
                  with NOT_UNIFIABLE -> iter ps
                )
        in iter prog

;;

let rec vars_term t =
  match t with
      V(v) -> [v]
    | Node(s, l) -> foldl union [] (map vars_term l)
    | _ -> []
;;

let vars_atom (A(s, l)) = vars_term (Node(s, l))
;;

let rec vars_goal (G(g)) = foldl union [] (map vars_atom g)
;;

let interpret_goal prog g = solve_goal prog g [] (vars_goal g)
;;
