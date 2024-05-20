(* Problem 1 *)
let f1 = fun acc (l, r) -> acc @ [(r, l)] 

let _ = List.fold_left f1 [] [(1, 2);(3, 4)];;
(* (int * int) list = [(2, 1); (4, 3)] *)


let f2 = fun acc x ->
  if (List.length acc) mod 2 = 0 then
    x::acc
  else
    acc @ [x]

let _ = List.fold_left f2 [] [1;2;3;4;5;6;7;8;9]
(* int list = [9; 7; 5; 3; 1; 2; 4; 6; 8] *)

let f3 acc (k, v) = 
  fun k' -> if k' = k then v else acc k'

let lookup_function = List.fold_left f3 (fun _ -> 0) [(0, 1); (2, 3)]


(* Problem 2 *)
(* 
  let rec map f = function
    | [] -> []
    | x :: xs -> f x :: map f xs

  let rec replicate n x =
    if n < 1 then [] else x :: replicate (n-1) x
*)

let map_tr f lst =
  let rec aux f acc = function
    | [] -> acc
    | h::tl -> aux f ((f h)::acc) tl
  in List.rev @@ aux f [] lst

let replicate n x =
  let rec aux acc n' =
    if n' < 1 then acc
    else aux (x::acc) (n' - 1)
  in aux [] n


(* Problem 3 *)
type 'a ocaml_llist = 'a ocaml_cell Lazy.t 
  and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

type 'a custom_llist = (unit -> 'a custom_cell) 
  and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)

let rec map_over_ocaml_llist f llist = lazy (
  match Lazy.force llist with
    | NilO -> NilO
    | ConsO (h, tl) -> ConsO (f h, map_over_ocaml_llist f tl)
)

let rec map_over_custom_llist f llist = fun () ->
  match llist () with
    | NilC -> NilC
    | ConsC (h, tl) -> ConsC (f h, map_over_custom_llist f tl)


(* Problem 4 *)
let rec merge_ocaml_llists l1 l2 = lazy (
  let forced_l1 = Lazy.force l1 in
  let forced_l2 = Lazy.force l2 in
  match forced_l1, forced_l2 with
    | NilO, NilO -> NilO
    | NilO, _ -> forced_l2
    | _, NilO -> forced_l1
    | ConsO (h1, tl1), ConsO (h2, tl2) ->
      if h1 <= h2 then 
        ConsO (h1, merge_ocaml_llists tl1 l2)
      else 
        ConsO (h2, merge_ocaml_llists tl2 l1)
)


let rec merge_custom_llists l1 l2 =
  let extracted_l1 = l1 () in
  let extracted_l2 = l2 () in
  match extracted_l1, extracted_l2 with
    | NilC, _ -> l2
    | _, NilC -> l1
    | ConsC (h1, tl1), ConsC (h2, tl2) ->
      if h1 <= h2 then
        fun () -> ConsC (h1, merge_custom_llists tl1 l2)
      else
        fun () -> ConsC (h2, merge_custom_llists tl2 l1)


(* Problem 5 *)
let rec drop_dupl_ocaml_llists llist = lazy (
  match Lazy.force llist with
  | NilO -> NilO
  | ConsO (h, tl) ->
    match Lazy.force tl with
      | NilO -> ConsO (h, lazy NilO)
      | ConsO (h', tl') ->
        if h = h' then
          Lazy.force @@ drop_dupl_ocaml_llists tl
        else
          ConsO (h, drop_dupl_ocaml_llists tl)
)


let rec drop_dupl_custom_llists llist = 
  (* Wrap the operation as fun (equivalent to lazy keyword) *)
  fun () ->
    match llist () with
    | NilC -> NilC
    | ConsC (h, tl) ->
        match tl () with
        | NilC -> ConsC (h, fun () -> NilC)
        | ConsC (h', tl') ->
            if h = h' then
              (* if neighbour duplicates found, omit the current one and continue recursion with tail *)
              drop_dupl_custom_llists tl ()
            else
              (* else, remain an element and continue searching duplicates in tail *)
              ConsC (h, drop_dupl_custom_llists tl)


(* Problem 6 *)
(* Scaling helpers for scaling factors for generating hamming numbers. *)
let rec scale_ocaml factor l = lazy (
  match Lazy.force l with
  | NilO -> NilO
  | ConsO (h, tl) -> ConsO (factor * h, scale_ocaml factor tl)
)

let rec scale_custom factor l = 
  fun () -> 
    match l () with
    | NilC -> NilC
    | ConsC (h, tl) -> ConsC (factor * h, scale_custom factor tl)

let rec hamming_ocaml_llist = lazy (
  ConsO (
    1,
    merge_ocaml_llists 
      (scale_ocaml 2 hamming_ocaml_llist) 
      (merge_ocaml_llists 
        (scale_ocaml 3 hamming_ocaml_llist) 
        (scale_ocaml 5 hamming_ocaml_llist)
      )
  )
)

let rec hamming_custom_llist = fun () ->
  ConsC (
    1, 
    merge_custom_llists
      (scale_custom 2 hamming_custom_llist) 
      (merge_custom_llists 
        (scale_custom 3 hamming_custom_llist) 
        (scale_custom 5 hamming_custom_llist))
      )