type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Construct Completely Balanced Binary Trees *)
(* TODO *)
let cbal_tree n = []

(* Symmetric Binary Trees *)
let is_symmetric tree =
  let rec is_mirror l r = match (l, r) with
    | Empty, Empty -> true
    | Node (_, l, r), Node (_, l', r') ->
      is_mirror l r' && is_mirror r l'
    | _ -> false
  in match tree with
    | Empty -> true
    | Node (_, l, r) -> is_mirror l r

  
(* Binary Search Trees (Dictionaries) *)
let construct lst = 
  let rec insert tree el = match tree with
  | Empty -> Node (el, Empty, Empty)
  | Node (el', l, r) ->
    if el' = el then tree
    else if el < el' then Node (el', insert l el, r)
    else Node (el', l, insert r el)
  in List.fold_left insert Empty lst


let _ = is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);;
let _ = not (is_symmetric (construct [3; 2; 5; 7; 4]));;


(* Generate-and-Test Paradigm *)
let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n)

let _ = List.length (sym_cbal_trees 57)


(* Construct Height-Balanced Binary Trees *)
(* Construct Height-Balanced Binary Trees With a Given Number of Nodes *)


(* Count the Leaves of a Binary Tree *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) ->  count_leaves l + count_leaves r


(* Collect the Leaves of a Binary Tree in a List *)
let leaves tree = 
  let rec aux acc = function
    | Empty -> acc
    | Node (e, Empty, Empty) -> e::acc
    | Node (e, l, r) -> aux (aux acc r) l
  in aux [] tree


(* Collect the Internal Nodes of a Binary Tree in a List *)
let internals tree = 
  let rec aux acc = function
    | Empty -> acc
    | Node (e, Empty, Empty) -> acc
    | Node (e, l, r) -> aux (e :: (aux acc r)) l
  in aux [] tree


(* Collect the Nodes at a Given Level in a List *)
let at_level tree n =
  let rec aux acc n' = function
    | Empty -> acc
    | Node (e, l, r) -> 
      if n = n' then e::acc
      else aux (aux acc (n' + 1) r) (n' + 1) l
  in aux [] 1 tree