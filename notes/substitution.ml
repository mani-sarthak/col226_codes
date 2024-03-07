open List;; 
type symbol = string * int;; 
type signature = symbol list;;


let sig1 = [ ("0", 0); ("1", 0); ("+", 2); ("*", 2) ];;


type tree = 
  V of string 
  | C of {node: symbol; children: tree list};; 



let x = V "x";; 
let y = V "y";; 
let z = V "z";; 
let zero = C {node = ("0", 0); children = []};; 
let one = C {node = ("1", 0); children = []};; 
let plus_zero_one = C {node = ("+", 2); children = [zero; one] };; 
let times_one_x = C {node = ("*", 2); children = [one; x] };; 
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };; 
let plus_timesonex_pluszeroy = 
  C {node = ("+", 2); 
  children = [times_one_x; plus_zero_y ] };; 
let plus_timesonex_z = 
  C {node = ("+", 2); 
  children = [times_one_x; z ] };; 



let rec ht t = match t with
  V _ -> 0 
  | C r -> 
  let (s,n) = r.node 
    in (if n = 0 
      then 0 
      else 1+(fold_left max 0 (map ht r.children) 
      )) 
 ;;

ht zero;; 
ht x;; 
ht plus_zero_one;; 
ht plus_timesonex_pluszeroy;; 
ht plus_timesonex_z ;;



let rec size t = match t with
  V _ -> 1 
 | C r -> 1+(fold_left (+) 0 (map size r.children) ) 
;;


size zero;; 
size x;; 
size plus_zero_one;; 
size plus_timesonex_pluszeroy;; 
size plus_timesonex_z ;; 


let update_lst lst x = if (mem x lst) then (lst) else (x::lst);;

let rec vars lst t = match t with 
  V v -> [v]
 | C r -> 
 lst @ (fold_left (vars) [] r.children)

let acc = [] ;;
vars acc zero;; 
vars acc x;; 
vars acc plus_zero_one;; 
vars acc plus_timesonex_pluszeroy;; 
vars acc plus_timesonex_z ;; 

let rec subst sigma t = match t with
 V x -> sigma x 
 | C r -> C { node = r.node; 
 children = map (subst sigma) r.children } 
;; 


(* let rec subst sigma t = match t with
  | V v -> (try List.assoc v sigma with Not_found -> t)
  | C r -> C {r with children = List.map (subst sigma) r.children};; *)




(* let rec apply_subst (sigma: substitution) (t: tree) : tree = match t with
  | V v -> (match List.assoc_opt v sigma with
            | Some t' -> t'
            | None -> t)
  | C {node; children} -> C {node; children = List.map (apply_subst sigma) children};; *)


let id_sub v = V v;;



subst id_sub zero ;; 
subst id_sub one;; 
subst id_sub x;; 
subst id_sub y;; 
subst id_sub z;; 
subst id_sub plus_timesonex_pluszeroy ;; 
subst id_sub plus_timesonex_z ;;



let sigma1 v = match v with
 "x" -> one 
 | "y" -> plus_timesonex_pluszeroy 
 | _ -> V v 
;; 



subst sigma1 zero ;; 
subst sigma1 one;; 
subst sigma1 x;; 
subst sigma1 y;; 
subst sigma1 z;; 
subst sigma1 plus_timesonex_pluszeroy ;; 
subst sigma1 plus_timesonex_z ;;


let compose_subst s1 s2 t = subst s2 (subst s1 t);;

(* 
let compose_subst s1 s2 : substitution =
  (List.map (fun (var, t) -> (var, subst s2 t)) s1) @
  (List.filter (fun (var, _) -> not (List.exists (fun (v, _) -> v = var) s1)) s2);;
   *)

(* let rec mgu t1 t2 =
  match (t1, t2) with
  | (V v, _) when not (occurs v t2) -> if t1 = t2 then [] else [(v, t2)]
  | (_, V v) when not (occurs v t1) -> if t1 = t2 then [] else [(v, t1)]
  | (C {node = n1; children = c1}, C {node = n2; children = c2}) when n1 = n2 ->
      (try
         List.fold_left2 (fun acc_subst child1 child2 ->
           let child_mgu = mgu (subst acc_subst child1) (subst acc_subst child2) in
           acc_subst @ child_mgu
         ) [] c1 c2
       with Invalid_argument _ -> raise NOT_UNIFIABLE)
  | _ -> raise NOT_UNIFIABLE;; *)