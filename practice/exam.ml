(* Exercise 3 *)
type 'a one_two = Null | One of 'a * 'a one_two | Two of 'a one_two * 'a * 'a one_two


(* ------------ extract_min ------------ *)
let rec extract_min = function
  | Null -> (None, Null)
  | One (x, Null) -> (Some x, Null)
  | One (x, t) ->
    let min, rest = extract_min t in
    (min, One (x, rest))
  | Two (Null, x, r) -> (Some x, r)
  | Two (l, x, r) ->
    let min, rest = extract_min l in
    (min, Two (rest, x, r))


(* ------------ verify ------------ *)
let verify tree =
  let rec aux lower upper is_descending = function
    | Null -> true
    | One (a, t) ->
        if is_descending then
          (* `a` should be less than the upper bound for descending order *)
          a < upper && aux lower a is_descending t
        else
          (* `a` should be greater than the lower bound for ascending order *)
          lower < a && aux a upper is_descending t
    | Two (t1, a, t2) ->
        aux lower a is_descending t1 && (if is_descending then a < upper else lower < a) && aux a upper is_descending t2
  in
  aux min_int max_int false tree
    
(* ------------ normal ------------ *)
let rec normal = function
  | Null -> Null
  | Two (l, a, r) -> Two (normal l, a, normal r)
  | One (a, t) -> Two (Null, a, normal t)


(* ------------ from_list ------------ *)

let from_list lst =
  let rec insert tr el = match tr with
  | Null -> One (el, Null)
  | One (a, t) ->
    if el < a then
      Two (One (el, Null), a, Null)
    else 
      Two (Null, a, One (el, Null))
  | Two (l, a, r) -> 
    if el < a then
      Two (insert l el, a, r)
    else
      Two (l, a, insert r el)
  in 
    let unique_sorted_lst = List.sort_uniq compare lst 
  in 
  List.fold_left insert Null unique_sorted_lst


(* ------------ remove ------------ *)
let rec remove tree el = match tree with
  | Null -> (false, Null)
  | One (a, t) ->
    if a = el then
      (true, t)
    else
      let (found, new_t) = remove t el in
      (found, One (a, new_t))
  | Two (l, a, r) ->
    if el = a then
      match l, r with
      | Null, _ -> (true, r)
      | _, Null -> (true, l)
      | _, _ ->
        let rec promote_min = function
          | One (b, r') -> (b, r')
          | Two (r1, b, r2) ->
            let (min, new_r1) = promote_min r1 in
            (min, Two (new_r1, b, r2))
          | Null -> failwith "Case is impossible, just added for pattern-matching"
        in
        let (min, new_r) = promote_min r in
        (true, Two (l, min, new_r))
    else if el > a then
      let (found, new_r) = remove r el in
      (found, Two (l, a, new_r))
    else
      let (found, new_l) = remove l el in
      (found, Two (new_l, a, r))


(* Exercise 4 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

(* It's very inneficient, but solution was started in that direction from problem description *)
let rec list_of_tree_post tr =
  match tr with
  | Br (label, l, r) -> (list_of_tree_post l) @ (list_of_tree_post r) @ [label]
  | Lf -> []


(* Efficient version by avoiding @ operation which is O(n) and using O(1) :: operation *)
let list_of_tree_post' tr =
  let rec aux acc = function
    | Lf -> acc
    | Br (label, l, r) -> aux (aux (label :: acc) r) l
  in aux [] tr


(* Test *)
let tree = Br('a', Br('b', Br('d', Lf, Lf), Br('e', Lf, Lf)), Br('c', Lf, Br('f', Br('g', Lf, Lf), Lf)))

let () =
  assert (list_of_tree_post tree = ['d'; 'e'; 'b'; 'g'; 'f'; 'c'; 'a']);
  assert (list_of_tree_post' tree = ['d'; 'e'; 'b'; 'g'; 'f'; 'c'; 'a'])