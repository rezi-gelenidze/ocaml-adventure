(* Exercise 1: Twice, no argumnets *)

(* double : int -> int *)
let double x = 2 * x

(* square : int -> int *)
let square x = x * x

(* twice : ('a -> 'a) 'a -> 'a *)
let twice f x = f (f x)

(* quad : int -> int *)
let quad = twice double

(* fourth : int -> int *)
let fourth = twice square

(* 
As for last two examples, they are partially applied
functions, where first function is specified and type
is infered for second argument and return value.   
*)


(* Exercise 2: Mystery operator 1

let ( $ ) f x = f x
Applies right side value to left side function
  - square $ 2 + 2 will first be equivalent to square(2 + 2) 
  - square 2 + 2 is equivalent to square(2) + 2,
  *this mistery operator is same as @@ built-int operator
*)


(* Exercise 3: Mystery operator 2

let ( @@ ) f g x = x |> g |> f
Applies value x as composition f(g(x))
*)

(* Exercise 4: repeat *)
let repeat f n x =
  let rec repeat_aux n acc =
    if n <= 0 then acc
    else repeat_aux (n - 1) (f acc)
  in repeat_aux n x


(* Exercise 5: product *)
let product_left lst = List.fold_left (fun acc a -> acc *. a) 1.0 lst
let product_right lst = List.fold_right (fun a acc -> acc *. a) lst 1.0 


(* Exercise 6: terse product *)
open ListLabels
let product_left_terse = List.fold_left ( *. ) 1.0
let product_right_terse = List.fold_right ~f:( *. ) ~init:1.0


(* Exercise 7: sum_cube_odd *)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l)

let (--) i j = from i j []

let sum_cube_odd n = 
  List.fold_left 
  (fun acc x -> acc +. float_of_int x ** 3.) 
  0.
  (List.filter (fun x -> x mod 2 = 1) (0 -- n))


(* Exercise 8: sum_cube_odd pipeline *)
let sum_cube_odd_pipelined n = (0 -- n) 
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.fold_left (fun acc x -> acc +. float_of_int x ** 3.) 0.


(* Exercise 9: Exists *)
let rec exists_rec f = function
  | [] -> false
  | h::tl -> f h || exists_rec f tl

let exists_fold f lst = List.fold_left (fun acc x -> acc || f x) false lst

let exists_lib f lst = List.exists


(* Exercise 10: Account balance *)
let balance_fl balance debits = 
  List.fold_left (fun balance' debit -> balance' -. debit) balance debits
let balance_fr balance debits = 
  List.fold_right (fun debit balance' -> balance' -. debit) debits balance

let rec balance_rec balance = function
  | [] -> balance
  | h::tl -> balance_rec (balance -. h) tl


(* Exercise 11: Library uncurried *)
let uncurried_append (lst1, lst2) = List.append lst1 lst2

let uncurried_compare (ch1, ch2) = Char.compare ch1 ch2

let uncurried_max (a, b) = Stdlib.max a b


(* Exercise 12: Map composition *)
let map_composition f g lst = List.map (fun x -> x |> g |> f) lst


(* Exercise 13: More list fun *)
let filter_str_longer_than_2 lst = 
  List.filter (fun x -> String.length x > 3) lst

let increment_each_float_by_1 lst =
  List.map (fun x -> x +. 1.) lst

let join_words_with_sep strs sep =
  List.fold_left (fun acc a -> if acc = "" then a else acc ^ sep ^ a) "" strs


(* Exercise 14: association list keys *)
let uniques assoc_lst = assoc_lst |> List.map fst |> List.sort_uniq Stdlib.compare


(* Exercise 15: Valid matrix *)
let is_valid_matrix = function
  | [] -> false
  | h::[] when h = [] -> false
  | h::tl ->
    let h_len = List.length h
    in
    List.for_all (fun x -> List.length x = h_len) tl


(* Exercise 16: row vector add *)
let add_row_vectors vec1 vec2 = List.map2 ( + ) vec1 vec2


(* Exercise 17: Matrix add *)
let add_matrices mat1 mat2 = List.map2 add_row_vectors mat1 mat2 


(* Exercise 18: Matrix multiply *)
let rec matrix_transposition matrix =
  let rec transposition_aux acc = function
    | [] | [] :: _ -> List.rev acc
    | mat -> transposition_aux (List.map List.hd mat :: acc) (List.map List.tl mat)
  in transposition_aux [] matrix

let dot_product = List.fold_left2 (fun acc x y -> acc + x * y) 0

let multiply_matrices mat1 mat2 =
  List.map (fun row -> List.map (dot_product row) (matrix_transposition mat2)) mat1
