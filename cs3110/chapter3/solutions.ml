(* Exercise 1: list expressions *)

(* standard [] syntax *)
[1; 2; 3; 4; 5];;

(* :: operator *)
1::2::3::4::5::[];;

(* @ Concat operator *)
[1] @ [2; 3; 4] @ [5];;


(* Exercise 2: product *)
let rec product = function
  | [] -> 1
  | h::t -> h * product t

let () = assert (product @@ 2::3::5::[] = 30)
let () = assert (product [] = 1)


(* Exercise 3: concat *)
let rec concat = function
  | [] -> ""
  | h::t -> h ^ (concat t)


let () = assert (concat @@ "O"::"caml"::[] = "Ocaml")
let () = assert (concat @@ [] = "")


(* Exercise 4: product test *)
(* Testing Topic is ignored *)

(* Exercise 5: patterns *)
let has_bigred = function
  | "bigred"::t -> true
  | _ -> false

let has_len_2_or_4 = function
  | _::_::[] | _::_::_::_::[] -> true
  | _ -> false


let has_fst_two_equal = function
  | first::second::tail -> first = second
  | _ -> false


(* Exercise 6: library *)
let get_fifth lst = 
  if (List.length lst < 5) 
    then 0 
  else 
    List.nth lst 5;;

let sort_desc lst = 
  List.rev @@ List.sort Stdlib.compare lst


(* Exercise 7: library test *)
(* Testing Topic is ignored *)


(* Exercise 8: library puzzle *)
let get_last lst = List.nth lst @@ List.length lst - 1
  
let any_zeroes lst = List.exists (fun x ->  x = 0) lst


(* Exercise 9: take drop *)
(* function "from" used from lecture. generates list of i to j sequence *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

let take n lst = 
  let rec take_aux n lst = 
    match lst with
      (* Continue recursive taking, until n > 0 *)
      | h::tl when n > 0 -> h::(take_aux (n - 1) tl)
      (* Stop at the base case *)
      | _ -> []
  in take_aux n lst

let drop n lst =
  let rec drop_acc n lst =
    match lst with
      | h::tl when n > 0 -> drop_acc (n - 1) tl
      | _ -> lst
    in let result = drop_acc n lst
  in if result = [] && n > 0 then [] else result

(* Exercise 9: take drop tail *)
let take_tail n lst = 
  let rec take_aux n lst lst_acc = 
    match lst with
      (* Continue recursive taking, until n > 0 *)
      | h::tl when n > 0 -> take_aux (n - 1) tl (h::lst_acc)
      (* Stop at the base case *)
      | _ -> lst_acc
  in List.rev @@ take_aux n lst []

(* drop implementation was already a tail recursive. *)

(* Exercise 10: unimodal *)
let is_unimodal (lst : int list): bool =
  match lst with
    | [] -> true
    | h::tl ->
      let rec is_unimodal_aux lst prev_el is_increasing has_switched =
        match lst with
          | [] -> true
          | h::tl -> 
            if is_increasing then
              if prev_el <= h
                then is_unimodal_aux tl h is_increasing has_switched 
              else 
                if has_switched
                  then false
                else is_unimodal_aux tl h (not is_increasing) true 
            else
              if prev_el >= h
                then is_unimodal_aux tl h is_increasing has_switched
              else
                if has_switched
                  then false
                else is_unimodal_aux tl h (not is_increasing) true
    in is_unimodal_aux tl h true false

(* Exercise 11: powerset *)
(* After analyzing function recursive aspects, I stated the formula:
      P({x} ∪ S) = P(S) ∪ {A ∪ {x} | A ∈ P(S)}
  It can be interpreted as using root of the problem {} set,
  we can calculate final powerset by assembling it
  from {}, 1 element, 2 element ... n element powerset.
*)
let rec powerset (lst : int list) : int list list =
  match lst with
    | [] -> [[]]
    | h::tl -> 
      (* Calculate P(S) *)
      let p = powerset tl
    (* Calculate  {A ∪ {x} | A ∈ P(S)} and concat with P(S) *)
    in (List.map (fun x -> h::x) p) @ p


(* Exercise 12: print int list rec *)
let rec print_int_list (lst : int list) : unit =
  match lst with
  | [] -> ()
  | h::t ->
    print_endline @@ string_of_int h;
    print_int_list t


(* Exercise 13: print int list iter *)
let print_int_list' (lst : int list) : unit =
  List.iter (fun n -> print_endline @@ string_of_int n) lst


(* Exercise 13: student *)
type student = {
  first_name : string; last_name : string; gpa : float
  }


let rezi = {
  first_name = "Rezi"; last_name = "Gelenidze"; gpa = 3.07
}

let get_full_name = function
  | {first_name; last_name} 
    -> print_endline @@ first_name ^ " " ^ last_name


let create_student first_name last_name gpa = {
  first_name = first_name; last_name = last_name; gpa = gpa
}


(* Exercise 14: pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = {
  name: string; hp: int; ptype: poketype
}

let charizard = {
  name = "charizard"; hp = 78; ptype = Water
}

let squirtle = {
  name = "squirtle"; hp = 44; ptype = Water
}


(* Exercise 15: safe hd and tl *)
let safe_hd = function
  | [] -> None
  | h::t -> Some h

let safe_tl = function
  | [] -> None
  | _::t -> Some t


(* Exercise 16: pokefun *)
let max_hp (lst : pokemon list) : pokemon option =
  let rec max_accumulator lst' curr_max = 
    match lst' with
      | [] -> curr_max
      | h::tl ->
        match curr_max with
          | None -> max_accumulator tl (Some h)
          | Some curr ->
            if h.hp > curr.hp
              then max_accumulator tl (Some h)
            else max_accumulator tl curr_max
  in max_accumulator lst None


(* Exercise 17: date before *)
type date = int * int * int

let is_before date1 date2 = 
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
    (y1 < y2) || 
    (y1 = y2 && m1 < m2) || 
    (y1 = y2 && m1 = m2 && d1 < d2)


(* Exercise 18: earliest date *)
let earliest dates = 
  let rec earliest_acc dates' curr_earliest =
    match dates' with
    | [] -> curr_earliest
    | h::tl ->
      let new_earliest =
        match curr_earliest with
          | None -> Some h
          | Some curr_date ->
            if is_before h curr_date then Some h else Some curr_date
      in earliest_acc tl new_earliest
  in earliest_acc dates None

(* Exercise 19: assoc list *)
let insert k v lst = (k, v) :: lst

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t


let assoc_list = insert 3 "three" (
    insert 2 "two" (
      insert 1 "one" []
    )
  );;


let value2 = lookup 2 assoc_list
let value4 = lookup 4 assoc_list


(* Exercise 20: cards *)
type suit = Club | Diamond | Heart | Spade

type rank = Number of int | Jack | Queen | King | Ace

type card = {
  suit: suit; rank: rank
}

let ace_of_clubs = { suit = Club; rank = Ace }
let queen_of_hearts = { suit = Heart; rank = Queen }
let two_of_diamonds = { suit = Diamond; rank = Number 2 }
let seven_of_spades = { suit = Spade; rank = Number 7 }


(* Exercise 21: matching *)
(* Some x :: tl *)
(* [None, Some 1] *)

(* [Some 3110; None] *)
(* [None, Some 1] *)

(* [Some x; _] *)
(* [None, Some 1] *)

(* h1 :: h2 :: tl *)
(* [Some 1] *)

(* h :: tl *)
(* Impossible to bypass non empty list with this pattern *)

(* Exercise 22: quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) : sign =
  if x > 0 
    then Pos
  else if x < 0
    then Neg
  else
    Zero

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None


(* Exercise 23: quadrant when *)
let quadrant_when : int*int -> quad option = function
    | x, y when x > 0 && y > 0  -> Some I
    | x, y when x < 0 && y > 0 -> Some II
    | x, y when x < 0 && y < 0 -> Some III
    | x, y when x > 0 && y < 0 -> Some IV
    | _ -> None


(* Exercise 24: depth *)
type 'a tree = Leaf | Node of 'a * 'a tree  * 'a tree

let rec depth tree =
  match tree with
    | Leaf -> 0
    | Node (_, l, r) ->
      1 + max (depth l) (depth r)


(* Exercise 25: shape *)
let rec same_shape tree1 tree2 = 
  match (tree1, tree2) with
    | (Leaf, Leaf) -> true
    | (Node (_, ll, lr), Node (_, rl, rr)) ->
      (same_shape ll rl) && (same_shape lr rr)
    | _ -> false


(* Exercise 26: list max exn *)
let list_max (lst : int list) : int =
  match lst with
    | [] -> raise (Failure "empty")
    | fst::tail ->
      let rec max_accumulator lst' current_max =
        match lst' with
          | [] -> current_max
          | h::lst'' ->
            let new_max = 
              if h > current_max then h 
              else current_max
            in max_accumulator lst'' new_max
    in max_accumulator tail fst
  

(* Exercise 27: lsit max exn string *)
let list_max_string (lst : int list) : string =
  match lst with
    | [] -> "empty"
    | h::tl ->
      let rec max_accumulator lst' current_max =
        match lst' with
          | [] -> current_max
          | h::lst'' ->
            let new_max = 
              if h > current_max then h 
              else current_max
            in max_accumulator lst'' new_max
  in string_of_int @@ max_accumulator lst 0
  

(* Exercise 28: list max exn ounit *)
(* Testing Topic is ignored *)

(* Exercise 29: is_bst *)
(* 
  Invariant: For any node n, every node in the left subtree 
  of n has a value less than n's value, and every node in 
  the right subtree of n has a value greater than n's value. 
*)
type ('a, 'b) bst =
  | Leaf
  | Node of ('a * 'b) * ('a, 'b) bst * ('a, 'b) bst

type 'a bst_result =
  | Empty
  | Unsatisfied
  | Satisfied of 'a * 'a

let rec is_bst_aux tree curr_min curr_max =
  match tree with
    | Leaf -> Empty
    | Node ((k, _), l, r) ->
      (* check min and max bounds *)
      let left_valid = match curr_min with
        | Some min -> k > min
        | None -> true
      in 
      let right_valid = match curr_max with
        | Some max -> k < max
        | None -> true
      in
      if left_valid && right_valid then
        (* Consider all cases combination of children bst results *)
        match (is_bst_aux l curr_min (Some k), is_bst_aux r (Some k) curr_max) with
          | (Satisfied (l_min, _), Satisfied (_, r_max)) -> Satisfied (l_min, r_max)
          | (Empty, Satisfied (_, r_max)) -> Satisfied (k, r_max)
          | (Satisfied (l_min, _), Empty) -> Satisfied (l_min, k)
          | (Empty, Empty) -> Satisfied (k, k)
          | _ -> Unsatisfied
      else
        Unsatisfied

let is_bst tree =
  match is_bst_aux tree None None with
    | Satisfied _ -> true
    | _ -> false

(* Exercise 30: quadrant_poly *)
let sign_poly x =
  if x > 0  then `Pos
  else if x < 0 then `Neg
  else `Zero

let quadrant_poly = fun (x,y) ->
  match sign_poly x, sign_poly y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | _ -> None