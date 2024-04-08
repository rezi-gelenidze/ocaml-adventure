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

(* TODO *)

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

(* TODO *)


(* Exercise 8: library puzzle *)
let get_last lst = List.nth lst @@ List.length lst - 1
  
let any_zeroes lst = List.exists (fun x ->  x = 0) lst


(* Exercise 9: take drop *)
(* TODO *)


(* Exercise 9: take drop tail *)
(* TODO *)


(* Exercise 10: unimodal *)
(* TODO *)


(* Exercise 11: powerset *)
(* TODO *)


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
(* TODO *)


(* Exercise 17: date before *)
type date = int * int * int

let is_before date1 date2 = 
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
    (y1 < y2) || 
    (y1 = y2 && m1 < m2) || 
    (y1 = y2 && m1 = m2 && d1 < d2)


(* Exercise 18: earliest date *)
(* TODO *)


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

(* [Some 3110; None] *)

(* [Some x; _] *)

(* h1 :: h2 :: tl *)

(* h :: tl *)


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
(* TODO *)


(* Exercise 25: shape *)
(* TODO *)


(* Exercise 26: list max exn *)


(* Exercise 27: lsit max exn string *)


(* Exercise 28: list max exn ounit *)


(* Exercise 29: is_bst *)


(* Exercise 30: quadrant_poly *)
(* TODO *)