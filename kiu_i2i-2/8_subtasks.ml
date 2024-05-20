(* Exercise 1 *)
let rec member c t = function
  | [] -> false
  | h::tl ->
    if (c t h) = 0 then true
    else member c t tl


let equal_second_components (_, x) (_, y) = compare x y
let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)


(* Exercise 2 *)
let rec update_record element = function
  (* We approached the end, so record is absent and we need to add one *)
  | [] -> [(element, 1)]
  (* If we find a record of element, increment a record and stop recursion *)
  | (k, c)::tl when k = element -> (k, c + 1)::tl
  (* If not found, continue recursion in tail *)
  | h::tl -> h::(update_record element tl)

let count_occurrences lst =
  let rec aux acc = function
    | [] -> acc
    | h::tl ->
      aux (update_record h acc) tl
  in List.sort (fun (_, c1) (_, c2) -> compare c2 c1) (aux [] lst)


(* Exercise 3 *)
let rec drop_last = function
  | [] -> []
  | h::[] -> []
  | h::tl -> h::(drop_last tl)


(* Exercise 4 *)
let rec drop_last_opt = function
  | [] -> None
  | h::[] -> None
  | h::tl -> Some (h::(drop_last tl))


(* Exercise 5 *)
let zip_with bif l1 l2 =
  let rec aux acc l1' l2' =
    match l1', l2' with
      | h1::tl1, h2::tl2 ->
        aux ((bif h1 h2)::acc) tl1 tl2
      | _ -> acc
    in List.rev @@ aux [] l1 l2


(* Exercise 6 *)
let unzip lst = 
  List.fold_right (fun (a, b) (a_acc, b_acc) -> (a::a_acc, b::b_acc)) lst ([], []) 


(* Exercise 7 *)
(* 
[We use fold_right to avoid List.rev need in the end, so we start accumulation (folding) from right]

unzip [('a',1);('b',2)]

first call:
  ([], []) ('b', 2) -> (['b'], [2])
second:
  (['b'], [2]) ('a', 1) -> (['a', 'b'], [1, 2]) 
result: (['a', 'b'], [1, 2])
*)

(* Exercise 8 *)
(* TODO *)