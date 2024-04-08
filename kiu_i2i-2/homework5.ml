(* Assignment 1: More Students! *)

(* Starter code from previous assignment *)
type student = {
  first_name: string;
  last_name: string;
  id: int;
  semester: int;
  grades: (int * float) list;
}


type database = student list
let student_db : database = [];;

let rec find_student student_id db = 
  match db with
  | [] -> None
  | student::db' ->
    if student.id = student_id
      then Some student
    else
      find_student student_id db'


(* 1.1 
  remove_by_id : int -> database -> database 
  removes the student with the given id from the database. 
*)
let rec remove_by_id target_id db =
  match db with
    | [] -> []
    | student::db' ->
      if student.id = target_id
        then db'
      else
        student::(remove_by_id target_id db')

(* 1.2
  count_in_semester : int -> database -> int 
  counts the number of students in the given semester. 
*)
let rec count_in_semester target_semester db =
  match db with
    | [] -> 0
    | student::db' ->
      if student.semester = target_semester
        then 1 + count_in_semester target_semester db'
      else
        count_in_semester target_semester db'

(* 1.3
  student avg grade : int -> database -> float
  computes the average grade of the student with the given id.
  If no student with the given id exists or the student does 
  not have any grades, the function shall return 0.0.
*)
let avg_grade grades =
  (* Add mediator function to efficiently accumulate sum & count on 1 iteration *)
  let rec avg_acummulator curr_grades curr_sum curr_count =
    match grades with
      | [] -> curr_sum, curr_count
      | (_, grade)::grades' ->
        avg_acummulator grades' (curr_sum +. grade) (curr_count + 1)
    in let sum, count = avg_acummulator grades 0. 0
  in if count = 0 then 0. else sum /. float_of_int count


let rec student_avg_grade target_id db =
  match find_student target_id db with
    | None -> 0.
    | Some student -> avg_grade student.grades
        

(* 1.4
  course_avg_grade : int -> database -> float 
  computes the average grade achieved in the given course. 
  If no grades in the given course exist, the function shall return 0.0.
*)
(* Helper 1: Find all grades and group in a list of floats for further avg calculation *)
let rec find_course_in_student_grades (target_course_id) (grades: (int * float) list) =
  match grades with
    | [] -> None
    | (course_id, grade)::grades' ->
      if course_id = target_course_id
        then Some grade
      else find_course_in_student_grades target_course_id grades'

(* Helper 2: Mediator accumulator, returns  *)
let rec accumulate_course_grades (target_course_id : int) (db : database) : float list =
  match db with
    | [] -> []
    | student::db' ->
      match find_course_in_student_grades target_course_id student.grades with
        | None -> []
        | Some grade -> grade::accumulate_course_grades target_course_id db'

let rec course_avg_grade (target_course_id : int) (db : database) : float = 
  let rec avg_accumulator curr_grades curr_sum curr_count = 
    match curr_grades with
      | [] -> curr_sum, curr_count
      | curr_grade::curr_grades' ->
        avg_accumulator curr_grades' (curr_sum +. curr_grade) (curr_count + 1)
    in let sum, count = avg_accumulator (accumulate_course_grades target_course_id db) 0. 0  
  in if count = 0 then 0. else sum /. float_of_int count


(* Assignment 2: List Mishmash *)
let rec interleave3 lst1 lst2 lst3 = match (lst1, lst2, lst3) with
  | el1::lst1', el2::lst2', el3::lst3' -> el1::el2::el3::(interleave3 lst1' lst2' lst3')
  | el1::lst1', el2::lst2', [] -> el1::el2::(interleave3 lst1' lst2' [])
  | el1::lst1', [], el3::lst3' -> el1::el3::(interleave3 lst1' [] lst3')
  | [], el2::lst2', el3::lst3' -> el2::el3::(interleave3 [] lst2' lst3')
  | el1::lst1, [], [] -> el1::(interleave3 lst1 [] [])
  | [], el2::lst2, [] -> el2::(interleave3 [] lst2 [])
  | [], [], el3::lst3 -> el3::(interleave3 [] [] lst3)
  | [], [], [] -> []


(* Assignment 3: OCamlfication *)
let foo (x : int) (y : int) (b : bool) : int =
  (* Conditional swapping *)
  let x, y = if x > y then y, x else x, y
    (* Define loop recursively and call it in the end *)
    in let rec increment x y b = match x, y with
      (* Loop body entry condition *)
      | x, y when x < y  
        ->  if b then increment (x + 1) y (not b)
            else increment x (y - 1) (not b)
      (* Loop exit (return x) *)
      | x, _ -> x
  in increment x y b

(* Assignment 4: Polynomial Party *)
let eval_poly (x : float) (polynomial : float list) : float =
  (* Get highest_power from the length of a list *)
  let highest_power = float_of_int (List.length polynomial - 1) 
    in let rec accumulator polynomial power curr_sum =
        match polynomial with
          | [] -> curr_sum
          | coefficient::polynomial' 
            -> accumulator polynomial' (power -. 1.) (curr_sum +. coefficient *. (x ** power)) 
      in accumulator polynomial highest_power 0.


let derive_poly (polynomial : float list) : float list =
  (* Get highest_power from the length of a list *)
  let highest_power = float_of_int (List.length polynomial - 1) 
    in let rec derivation_accumulator (target_poly : float list) (derived_poly : float list) power =
      match target_poly with
        | [] | _::[] -> derived_poly
        | coefficient::target_poly' 
          (* Power rule =  C*x^n = C*n*x^(n-1) *)
          -> derivation_accumulator target_poly' ((coefficient *. power)::derived_poly) (power -. 1.)
    in List.rev @@ derivation_accumulator polynomial [] highest_power
    

(* Longest Twins *)
(* TODO *)