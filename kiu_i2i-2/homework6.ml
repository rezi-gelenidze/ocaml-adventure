(* ----------------- Exercise 1: Peano Arithmetic ----------------- *)
type nat = Zero | Succ of nat

let rec int_to_nat n =
  if n = 0 then Zero
  else Succ (int_to_nat (n - 1))

let rec nat_to_int = function
  | Zero -> 0
  | Succ nat -> 1 + nat_to_int nat

let add a b = 
  let rec aux acc = function
    | Zero -> acc
    | Succ n -> Succ (aux acc n)
  in aux a b

let rec mul a b =
  match b with
    (* a * 0 = 0 *)
    | Zero -> Zero
    (* a * b = a + a * (b - 1) *)
    | Succ n -> add a (mul a n)

let rec pow a b =
  match b with
    (* a^0 = 1 *)
    | Zero -> Succ Zero
    (* a^n = a * a ^ (b - 1) *)
    | Succ n -> mul a (pow a n)

(* I love pattern-matching moment :) *)    
let rec leq a b =
  match a, b with
    | Zero, Zero -> true
    | Zero, _ -> true
    | _, Zero -> false
    | Succ a', Succ b' -> leq a' b'


(* ----------------- Exerice 2: Quadtrees ----------------- *)