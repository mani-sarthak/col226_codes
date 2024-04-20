(* Toy Interpreter for a Simple Prolog-like Language in OCaml *)

(* Data types for terms, formulas, and programs *)
type variable = string
type predicate = string
type constant = string
type term =
    | Var of variable
    | Const of constant
    | Pred of predicate * (term list)
type formula = Pred of predicate * (term list)
type clause =
    | Fact of formula
    | Rule of formula * (formula list)
type program = clause list
type goal = formula list

(* Helper function to check if an item is a member of a list *)
let rec is_member item list = List.mem item list

(* Convert a term to a string *)
let rec term_to_string = function
    | Const c -> c
    | Var v -> v
    | _ -> ""

(* Merge two lists while removing duplicates *)
let rec merge_lists l1 l2 = match l1 with
    | [] -> l2
    | head::tail ->
        if is_member head l2 then
            merge_lists tail l2
        else
            head :: (merge_lists tail l2)

(* Function to find if a variable occurs in a term *)
let rec occurs var = function
    | Var v -> v = var
    | Const _ -> false
    | Pred (_, terms) -> List.exists (occurs var) terms


let rec unify_terms t1 t2 = match (t1, t2) with
    | (Const a, Const b) -> if a = b then [] else raise Not_found
    | (Var v, Const c) | (Const c, Var v) -> [(v, Const c)]
    | (Const _, Pred _) | (Pred _, Const _) -> raise Not_found
    | (Var v1, Var v2) -> if v1 <> v2 then [(v1, Var v2)] else []
    |  (Var v, Pred (p, args) as pred) | (Pred (p, args) as pred, Var v) ->
        if not (occurs v t) then [(v, t)] else raise Not_found
    | (Pred (f1, lst1), Pred (f2, lst2)) ->
        if f1 = f2 && List.length lst1 = List.length lst2 then
            List.fold_left2 (fun acc x y -> merge_lists acc (unify_terms x y)) [] lst1 lst2
        else
            raise Not_found


(* Unify two formulas *)
let unify_formulas f1 f2 = match (f1, f2) with
    | (Pred (p1, t1), Pred (p2, t2)) -> 
        if p1 = p2 && List.length t1 = List.length t2 then
            List.fold_left2 (fun acc x y -> merge_lists acc (unify_terms x y)) [] t1 t2
        else
            raise Not_found
;;


(* Unify two clauses *)




(* Apply substitutions to a term *)
let rec apply_substitution term substitutions = match term with
    | Const c -> Const c
    | Var v -> (
      try 
      let _, t = List.find (fun (x, t) -> v = x) substitutions 
      in t
      with Not_found -> Var v
    )
    | Pred (pred, terms) -> Pred (pred, List.map (fun t -> apply_substitution t substitutions) terms)

(* Handle substitution in a body of a clause *)
let substitute_in_body body subs =
    List.fold_left (fun acc f -> merge_lists acc [apply_substitution f subs]) [] body

(* Compose two substitution lists *)
let rec compose_subs s1 s2 =
    let rec compose_with_acc s1 s2 acc = match (s1, s2) with
        | ([], []) -> acc
        | (l, []) | ([], l) -> merge_lists acc l
        | ((v1, t1) :: rest1), ((v2, t2) :: rest2) ->
            if occurs v2 t1 then
                compose_with_acc rest1 rest2
                    (merge_lists acc [(v1, apply_substitution t1 [(v2, t2)]); (v2, t2)])
            else
                compose_with_acc rest1 rest2 (merge_lists [(v1, t1); (v2, t2)] acc)
    in compose_with_acc s1 s2 []

let hd_bdy cls =
  match cls with
  | Fact hd -> (hd, [])
  | Rule (hd, bdy) -> (hd, bdy);;


  let rec solve_one program goal other_goals ans to_do =
    let rec resolve p g =
        match p with
        | [] -> false
        | cls::rest ->
            try
                let hd, bdy = hd_bdy cls in
                let subst = unify_formulas g hd in
                let new_goals = substitute_in_body (merge_lists other_goals bdy) subst in
                if solve program new_goals (compose_subs ans subst) to_do then
                    true
                else
                    resolve rest g
            with
                Not_found -> resolve rest g
    in resolve program goal
;;



(* Function to solve goals *)
let rec solve program goals substitutions todo =
    match goals with
    | [] -> true
    | goal :: rest ->
    solve_one program goal rest substitutions todo
;;


(* Extract variables from a term *)
let rec vars_of_term term = match term with
    | Const _ -> []
    | Var v -> [v]
    | Pred (_, terms) -> List.fold_left (fun acc t -> merge_lists acc (vars_of_term t)) [] terms

(* Entry point for the solver *)
let solver prog goals =
    let vars_needed = List.fold_left (fun acc g -> merge_lists acc (vars_of_term g)) [] goals in
    solve prog goals [] vars_needed
