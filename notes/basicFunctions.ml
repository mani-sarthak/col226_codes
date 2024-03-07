();;

true;;

false;;

not true;;

not false;;

true && false;;

false || true;;

not;;

(&&);;

0 ;;

1 ;;

0 + 1 ;;

"Hello, world!" ;;

't' ;;


"Hello" ^ " World!\n" ;;

type age = int ;;

type identity = string * age ;;


type myBool = T | F;;


T;;

type myBool' = T' of unit | F' of unit ;;


T'() ;;


let myBool2bool b = match b with
  T -> true
  | F -> false
;;



myBool2bool T;;


let bool2myBool b = match b with
true -> T
| false -> F
;;


let myNot b = match b with
T -> F
| F -> T
;;


let myAnd b1 b2 = match b1 with
T -> b2
| F -> F
;;


type nat = Z | S of nat;;

let zero = Z;;
let one = S Z;;
let two = S (S Z);;
let three = S (S (S Z));;


let nonzero n = match n with
Z -> false
| S _ -> true
;;



nonzero zero;;
nonzero three;;


let rec nat2int n = match n with
Z -> 0
| S x -> 1+(nat2int x)
;;
nat2int zero;;
nat2int one;;
nat2int two;;
nat2int three;;


let rec addnat m n = match m with
Z -> n (* 0+n = n *)
| S x -> S (addnat x n) (* (1+x) + n = 1 + (x+n) *)
;;
addnat zero three;;
addnat three zero;;
addnat one two;;
addnat two one;;


let rec multnat m n = match m with
Z -> Z (* 0 * n = 0 *)
| S x -> addnat n (multnat x n) (* (1+x) * n = n + (x*n)
*)
;;
multnat zero two;;
multnat two zero;;
multnat one three;;
multnat three one;;
multnat three two;;



let rec expnat m n = match n with
Z -> (S Z) (* m^0 = 1 *)
| S x -> multnat m (expnat m x) (* m^(1+x) = m * (m^x) *)
;;
expnat zero one;;
expnat zero zero;;
expnat zero three;;
expnat three zero;;
expnat two three;;
expnat two one;;
expnat three two;;


type 'a list = Nil | Cons of 'a * ('a list);;

[ ];;

1 :: [ ];;
1 :: (2 :: []);;


let rec length l = match l with
[ ] -> 0
| _ :: xs -> 1 + (length xs)
;;

length [ ];;
length [1;2;3];;


let rec append l1 l2 = match l1 with
[ ] -> l2
| x::xs -> x :: (append xs l2)
;;

append [ ] [1;2;3];;
append [1;2;3] [ ];;

let rec rev s = match s with
[ ] -> [ ]
| x::xs -> (rev xs) @ [x]
;;

rev [1;2 ;3];;


let rev s =
  let rec rev2 s1 s2 = match s1 with
  [ ] -> s2
  | x::xs -> rev2 xs (x::s2)
  in
  rev2 s [ ]
;;


rev [1;2; 3];;