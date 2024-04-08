(* 1.1.1 Create a function which calculates sum from 1 to N *)

(* classic *)
let rec sum n =
  if n = 1 then 1
  else n + sum (n - 1) 


(* tail-recursive *)
let rec sum_tail n =
  let rec aux n acc =
    if n = 0
      then acc
    else 
      aux (n - 1) (acc + n)
  in aux n 0


(* 1.1.2 Write a factorial function *)

(* classic *)
let rec fact n =
  if n = 1 then 1
  else n * fact (n - 1)

(* tail-recursive *)
let rec fact_tail n =
  let rec aux n acc =
    if n = 0
      then acc
    else 
      aux (n - 1) (n * acc)
  in aux n 1


(* 1.2 Create a record of type student which will have following fields 
  first_name: string; last_name: string; id: int; semester: int; grades: (int * float) list;
*)

type student = {
  first_name: string;
  last_name: string;
  id: int;
  semester: int;
  grades: (int * float) list;
}


(* 2 create a db of type student (list of students) *)
type database = student list
let student_db : database = [];;


(* 3 Create a function which will add a new student to the database *)
let add_student student_record = student_record::student_db


(* 4 Create a function which will find a student with a given id and db *)
let rec find_student student_id db = 
  match db with
  | [] -> None
  | student::db' ->
    if student.id = student_id
      then Some student
    else
      find_student student_id db'


(* 5 find all the students with given last_name from db *)
let rec find_with_last_name last_name db =
  match db with
    | [] -> []
    | student::db' ->
      if student.last_name = last_name
        then student::(find_with_last_name last_name db')
      else
        find_with_last_name last_name db'


(* 6 Remove student with given id from the db *)
let rec remove_by_id target_id db =
  match db with
    | [] -> []
    | student::db' ->
      if student.id = target_id
        then db'
      else
        student::(remove_by_id target_id db')


(* 7 Remove all the students from the db with given first_name *)
let rec remove_all_by_first_name target_first_name db =
  match db with
    | [] -> []
    | student::db' ->
      if student.first_name = target_first_name
        then remove_all_by_first_name target_first_name db'
      else
        student :: remove_all_by_first_name target_first_name db'


(* 8 find how many students are ingiven semester in the db *)
let rec count_in_semester target_semester db =
  match db with
    | [] -> 0
    | student::db' ->
      if student.semester = target_semester
        then 1 + count_in_semester target_semester db'
      else
        count_in_semester target_semester db'


(* 9 Calculate the average grade of the student with a given id *)
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

