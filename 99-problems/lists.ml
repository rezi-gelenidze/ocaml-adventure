(* Tail of a List *)
let rec last = function
  | [] -> None
  | [e] ->  Some e
  | _::tl -> last tl


(* Last Two Elements of a List *)
let rec last_two = function
  | [] -> None
  | [x1; x2] -> Some (x1, x2)
  | _::tl -> last_two tl


(* N'th Element of a List *)
let rec nth i = function
  | [] -> raise @@ Failure "Index out of bounds"
  | h::tl -> if i = 0 then h else nth (i - 1) tl


(* Length of a List *)
let lenth lst =
  let rec lenth_aux curr_count = function
    | [] -> curr_count
    | _::tl -> lenth_aux (curr_count + 1) tl
  in lenth_aux 0 lst


(* Reverse a List *)
let rev lst =
  let rec rev_aux curr_result = function
    | [] -> curr_result
    | h::tl -> rev_aux (h::curr_result) tl
  in rev_aux [] lst


(* Palindrome *)
let is_palindrome lst = lst = List.rev lst


(* Flatten a List *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst =
  let rec flatten_aux acc = function
    | [] -> acc
    | (One x)::tl -> flatten_aux (x::acc) tl
    | (Many m)::tl -> flatten_aux (flatten_aux acc m) tl
  in List.rev @@ flatten_aux [] lst


(* Eliminate Duplicates *)
let rec compress = function
  | x1 :: (x2 :: _ as tl) -> 
    if x1 = x2 then compress tl
    else x1 :: compress tl
  | lst -> lst


(* Pack Consecutive Duplicates *)
let pack lst =
  let rec aux acc current_sublist = function
    | [] -> (List.rev current_sublist) :: acc
    | [x] -> (List.rev (x :: current_sublist)) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then
        aux acc (x :: current_sublist) tl
      else
        aux ((x :: current_sublist) :: acc) [] tl
  in List.rev (aux [] [] lst)


(* Run-Length Encoding *)
let encode lst =
  let rec aux n acc = function
    | [] -> []
    | [x] -> (n + 1, x) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then
        aux (n + 1) acc tl
      else
        aux 0 ((n + 1, x)::acc) tl
  in List.rev @@ aux 0 [] lst


(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode' lst =
  let rec aux n acc = function
    | [] -> []
    | [x] ->
      if n = 0 then
        One x :: acc
      else
        Many (n + 1, x) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then
        aux (n + 1) acc tl
      else if n = 0 then
        aux 0 (One x :: acc) tl
      else
        aux 0 (Many (n + 1, x)::acc) tl
  in List.rev @@ aux 0 [] lst


(* Decode a Run-Length Encoded List *)
let decode lst = 
  let rec times n x acc =
    if n = 0 then acc
    else times (n - 1) x (x::acc)
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many (n, x) :: tl -> aux (times n x acc) tl
  in List.rev @@ aux [] lst


(* Run-Length Encoding of a List (Direct Solution) *)
let encode_direct lst =
  let rec aux count acc = function
    | [] -> []
    | [x] -> 
      if count = 0 then
        One x :: acc
      else
        Many (count + 1, x) :: acc
    | x :: (y :: _ as tl) -> 
      if x = y then 
        aux (count + 1) acc tl
      else if count = 0 then
        aux 0 (One x :: acc) tl
      else
        aux 0 (Many (count + 1, x) :: acc) tl
      in List.rev (aux 0 [] lst);;


(* Duplicate the Elements of a List *)
let rec duplicate = function
  | [] -> []
  | h::tl -> h :: h :: duplicate tl


(* Replicate the Elements of a List a Given Number of Times *)
let replicate lst n = 
  let rec times n x acc =
    if n = 0 then acc
    else times (n - 1) x (x::acc)
  in
  let rec aux acc = function
    | [] -> acc
    | h::tl -> aux (times n h acc) tl
in List.rev @@ aux [] lst


(* Drop Every N'th Element From a List *)
let drop lst n =
  let rec aux acc i = function
    | [] -> acc
    | h::tl -> 
        if i = n then
          aux acc (i + 1) tl
        else
          aux (h :: acc) (i + 1) tl
  in List.rev @@ aux [] 1 lst


(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split lst n =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | h::tl as l -> 
      if i = n then
        (List.rev acc, l)
      else
        aux (h::acc) (i + 1) tl
  in aux [] 0 lst


(* Extract a Slice From a List *)
(* Elegancy of Reusability: *)
let slice lst i k = fst @@ split (snd (split lst i)) (k - i + 1)


(* Rotate a List N Places to the Left *)
(* Elegancy of Reusability 2: *)
let rotate lst k = 
  let splitten = split lst (k mod List.length lst) 
  in snd splitten @ fst splitten


(* Remove the K'th Element From a List *)
let rec remove_at k = function
  | [] -> []
  | h::tl -> if k = 0 then tl else h :: remove_at (k - 1) tl


(* Insert an Element at a Given Position Into a List *)
let rec insert_at el i = function
  | [] -> [el]
  | h::tl -> if i = 0 then el::tl else h :: insert_at el (i - 1) tl;;


(* Create a List Containing All Integers Within a Given Range *)
let range i j =
  let rec aux i' j' =
    if i' > j' then [] else i' :: aux (i' + 1) j'
  in if i < j then aux i j else List.rev @@ aux j i


(* Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select lst n =
  Random.init 0;
  let len = List.length lst in
  let rec aux acc n =
    if n = 0 then acc
    else aux ((List.nth lst (Random.int len)) :: acc) (n - 1)
  in aux [] n


(* Lotto: Draw N Different Random Numbers From the Set 1..M *)
let rec lotto_select i j = rand_select (range 1 j) i


(* Generate a Random Permutation of the Elements of a List *)
let permutation lst =
  Random.self_init ();

  let len = List.length lst in

  let rec set_nth n el = function
    | [] -> failwith "Index out of bounds"
    | h::tl -> if n = 0 then el::tl else h :: set_nth (n - 1) el tl
  in

  let rec aux i lst =
    if i <= 0 then lst
    else
      let j = Random.int (i + 1) in

      let i_element = List.nth lst i in
      let j_element = List.nth lst j in

      let lst = set_nth i j_element lst in
      let lst = set_nth j i_element lst in

      aux (i - 1) lst
  in aux (len - 1) lst


(* ------------------ RIP My IQ Zone ------------------*)

(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
(* Group the Elements of a Set Into Disjoint Subsets *)

(* ------------------------ END -----------------------*)


(* Sorting a List of Lists According to Length of Sublists *)
(* 1. Length Sort *)
let predicate l1 l2 = compare (List.length l1) (List.length l2)
let length_sort lst = List.sort predicate lst

(* 2. Frequency Sort *)
(* Below sort predicate is not very efficient, but problem does not require time complexity reduction *)
let frequency_sort lst = 
  (* Define helper for counting given length occurances in a list *)
  let count_occurances length =
    let rec aux acc = function
      | [] -> acc
      | h::tl ->
        if List.length h = length 
          then aux (acc + 1) tl
          else aux acc tl
    in aux 0 lst
  in
  (* Define predicate, comparing number of length occurances *)
  let predicate l1 l2 = 
    compare (count_occurances (List.length l1)) (count_occurances (List.length l2)) 
  
  (* Finally, use List sort with our predicate *)
  in List.sort predicate lst
