open List;; 
type symbol = string * int;; 
type signature = symbol list;;
exception NOT_UNIFIABLE;;



type tree = 
  V of string 
  | C of {node: symbol; children: tree list}
;; 



(* ///////////////////////////////////////// *)
let rec check_sig_arity s = match s with
  | [] -> true
  | (a, b)::t -> if b < 0 
                  then false 
                  else check_sig_arity t
;;

let are_unique_symbols sig1 =
  let rec aux seen = function
    | [] -> true
    | (s, _)::rest ->
      if List.exists (fun (sym, _) -> sym = s) seen then
        false
      else
        aux ((s, 0)::seen) rest
  in
  aux [] sig1
                  

let check_sig sig1 =
  are_unique_symbols sig1 && check_sig_arity sig1;;



let valid_sig = [("0", 0); ("1", 0); ("+", 2); ("*", 2)];;
let invalid_sig1 = [("0", 0); ("0", 1); ("+", 2); ("*", 2)];; (* same symbol repeted*)
let invalid_sig2 = [("0", 0); ("0", 0); ("+", 2); ("*", 2)];; (* same symbol and arity duplicated*)
let invalid_sig3 = [("0", 0); ("1", 1); ("+", 2); ("*", -1)];; (* negative arity *)

assert (check_sig valid_sig = true);;
assert (check_sig invalid_sig1 = false);;
assert (check_sig invalid_sig2 = false);;
assert (check_sig invalid_sig3 = false);;



(* /////////////////////////////////////////////////////// *)
let arity_of_symbol sig1 s =
  List.fold_left (fun acc (sym, ar) -> if sym = s then Some ar else acc) None sig1;;

let rec wftree sig1 t =
  match t with
  | V _ -> true
  | C {node = (s, _); children} ->
    (match arity_of_symbol sig1 s with
     | Some expected_arity -> 
         if List.length children = expected_arity then
           List.for_all (wftree sig1) children
         else
           false
     | None -> false)
;;

let sig1 = [("0", 0); ("1", 0); ("+", 2); ("*", 2)];;
let valid_tree = C {node = ("+", 2); children = [
                        C {node = ("1", 0); children = []};
                        C {node = ("*", 2); children = [
                           C {node = ("0", 0); children = []};
                           C {node = ("1", 0); children = []}
                         ]}
                      ]}
;;
let invalid_tree = C {node = ("+", 2); children = [
                        C {node = ("1", 0); children = []}
                      ]}
;;

assert(wftree sig1 valid_tree = true);;
assert(wftree sig1 invalid_tree = false);;


(* //////////////////////////////////////////// *)
let rec ht t = match t with
  V _ -> 0 
  | C r -> 
  let (s,n) = r.node 
    in (if n = 0 
      then 0 
      else 1+(fold_left max 0 (map ht r.children) 
      )) 
;;

let rec size t = match t with
  V _ -> 1 
 | C r -> 1+(fold_left (+) 0 (map size r.children) ) 
;;



let rec collect_vars t = match t with
  | V x -> [x]
  | C r -> List.fold_left (fun acc child -> acc @ (collect_vars child)) [] r.children;;

let remove_duplicates lst =
  let rec remove acc = function
    | [] -> List.rev acc
    | h::t -> remove (if List.mem h acc then acc else h::acc) t
  in remove [] lst;;

let vars t = remove_duplicates (collect_vars t);;


let vars_tree = C {node = ("+", 2); children = [
  C {node = ("1", 0); children = []};
  C {node = ("*", 2); children = [
      V "x";
      C {node = ("1", 0); children = []}
    ]}
]};;

let dup_vars_tree = C {node = ("+", 2); children = [
                        V "x";
                        C {node = ("*", 3); children = [
                           V "x"; 
                           V "y";
                           C {node = ("1", 0); children = []}
                         ]}
                      ]}
;;


assert(ht vars_tree = 2);;
assert(ht dup_vars_tree = 2);;
assert(size vars_tree = 5);;
assert(size dup_vars_tree = 6);;
assert(vars vars_tree = ["x"]);;
assert(vars dup_vars_tree = ["x"; "y"]);;




let rec print_tree t = match t with
  | V x -> Printf.printf "V \"%s\" " x
  | C {node = (s, _); children} ->
      Printf.printf "C \"%s\" [" s;
      List.iter print_tree children;
      Printf.printf "]";
;;

(* ////////////////////////////////////////////////// *)
let rec mirror t = match t with
  | V _ -> t
  | C r -> C {r with children = List.rev_map mirror r.children};;

let original_tree = C {node = ("+", 2); children = [
                        V "x";
                        C {node = ("*", 2); children = [
                           V "y";
                           V "z";
                         ]}
                      ]}
;;

let mirrored_tree = mirror original_tree;;

assert(mirror original_tree = C {node = ("+", 2); children = [
                        C {node = ("*", 2); children = [
                           V "z";
                           V "y";
                         ]};
                        V "x"
                      ]})
;;



(* ////////////////////////////////////////////////////// *)

type substitution = (string * tree) list;;
type subst_composition = substitution list;;


(* ////////////////////////////////////////////////////// *)

let one = C {node = ("1", 0); children = []};; 
let x = V "x";; 
let y = V "y";; 
let zero = C {node = ("0", 0); children = []};; 
let times_one_x = C {node = ("*", 2); children = [one; x] };; 
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };; 
let plus_timesonex_pluszeroy = 
  C {node = ("+", 2); 
  children = [times_one_x; plus_zero_y ] };;


let rec subst sigma t = match t with
  | V v -> (try List.assoc v sigma with Not_found -> t)
  | C r -> C {r with children = List.map (subst sigma) r.children};;


let apply_composed_substs compositions t =
  List.fold_left (fun acc_tree sigma -> subst sigma acc_tree) t compositions;;

let sigma1 = [("x", C {node = ("1", 0); children = []})];; 
let sigma2 = [("y", C {node = ("+", 2); children = [V "x"; V "y"]})];;

let example_tree = V "y";;


let result_tree = apply_composed_substs [sigma1] example_tree;;
assert (result_tree = example_tree);;


let result_tree = apply_composed_substs [sigma2] example_tree;;
assert (result_tree = C {node = ("+", 2); children = [V "x"; V "y"]});;

let composed_substs = [ sigma2; sigma1];;
let result_tree = apply_composed_substs composed_substs example_tree;;
assert (result_tree = C {node = ("+", 2); children = [C {node = ("1", 0); children = []}; V "y"]})
;;


let composed_substs = [ sigma2; sigma1; sigma2; sigma1];;
let result_tree = apply_composed_substs composed_substs example_tree;;
assert (result_tree = C {node = ("+", 2); children = [C {node = ("1", 0); children = []};
   C {node = ("+", 2); children = [C {node = ("1", 0); children = []}; V "y"]}]})
;;



(* ///////////////////////////////////////////////////////// *)

let rec occurs v t = match t with
  | V var -> var = v
  | C {children} -> List.exists (occurs v) children;;



let rec mgu t1 t2 = 
  if t1 = t2 then []   (* case 5 *)
  else match (t1, t2) with
  | (V v1, V v2) when v1 = v2 -> [] (* case 1 *)
  | ((V v, t) | (t, V v)) -> (* case 2, 3 and 4 with "occours check" *)
      if occurs v t then raise NOT_UNIFIABLE
      else [(v, t)]
  | (C {node = n1; children = c1}, C {node = n2; children = c2}) when n1 = n2 ->
      (try   (*case 9 *)
         List.fold_left2 (fun acc_subst child1 child2 ->
           let child_mgu = mgu (subst acc_subst child1) (subst acc_subst child2) in
           acc_subst @ child_mgu
         ) [] c1 c2
       with Invalid_argument _ -> raise NOT_UNIFIABLE) (* case 7 *)
  | _ -> raise NOT_UNIFIABLE;; (* case 6 and 8 *)
  

(* 
1. tree t and u are both variables and they are the same variables.
2. tree t and u are variables but they are different variables
3. tree t is a variable but tree u is a constant
4. tree t is a variable but tree u is a node (occours check should be handeled in this)
5. tree t is a constant and tree u is the same constant 
6. tree t is a constant but tree u is another constant (the exception must be raised in this)
7. tree t is a constant but tree u is a non leaf node (it must fail in this as well)
8. tree t is a node and tree u is node too but the symbols are different (must fail here as well)
9. tree t is a node and tree u is node too, they have the same symbols (in this case it must compute for the child trees mgus sequentially and then club it). 
*)


let x = V "x";;
let y = V "y";;
let z = V "z";;
let a = C {node = ("A", 0); children = []};;  
let b = C {node = ("B", 0); children = []};; 
let f_x = C {node = ("F", 1); children = [x]};; 
let g_y = C {node = ("G", 1); children = [y]};; 
let f_g_y = C {node = ("F", 1); children = [g_y]};; 
  
(* ////////////////////////////////////////////////////// *)

let test1 = mgu x x;;
assert (test1 = []);;
let test2 = mgu x y;;
assert (test2 = [("x", V "y")]);;
let test3 = mgu x a;; 
assert (test3 = [("x", a)]);;
(* let test4 = mgu x f_x;; *)
let test5 = mgu a a;;
assert(test5 = []);;
(* let test6 = mgu a b;; *)
(* let test7 = mgu a f_x;; *)
(* let test8 = mgu f_x g_y;; *)
let test9 = mgu f_x f_g_y;;
assert(test9 = [("x", g_y)]);;


(* ///////////////////////////////////////////////////////////// *)



(* mirror (mgu t u) = mgu (mirror u) (mirror t);; *) 
(* 
NOTE :- this is the correct statement since we have a case on which the statemnt 
  "mgu (t, u) = mgu (mirror u, mirror t)." fails.
  Consider :- t = V x 
              u = C {node = ("+", 2); children = [V "a";  V "b"]}

  mgu (t, u) = [("x", C {node = ("+", 2); children = [V "a";  V "b"]})]
  mirror t = V x
  mirror u = C {node = ("+", 2); children = [V "b";  V "a"]}
  mgu (mirror u, mirror t) = [("x", C {node = ("+", 2); children = [V "b";  V "a"]})]
  Hence mgu (t, u) != mgu (mirror u, mirror t) .


Proof Sketch :- 

      given t and u say sigma is the mgu of t and u, then sigma(t) = sigma(u)
      appling mirror we have mirror(sigma(t)) = mirror(sigma(u)).
      we have mirror(sigma(t)) = sigma(mirror(t)). (fairly easy to show)
      using it we have sigma(mirror(t)) = sigma(mirror(u)).
      hence sigma is a unifier of mirror(t) and mirror(u).
      Since sigma is a unifier of mirror(t) and mirror(u), and by the definition of mgu, 
      there cant be a more general substitution than sigma that unifies mirror(t) and mirror(u), 
      otherwise would contradict the fact that sigma is mgu of t and u.
      Hence we say mirror(sigma) is a mgu of mirror(t) and mirror(u).

 *)
