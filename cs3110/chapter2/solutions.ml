(* Exercise 1. Values *)

7 * (1 + 2 + 3);;
(* int = 42 *)

"CS " ^ string_of_int 3110;;
(* string = "CS 3110" *)


(* Exercise 2. Operators *)

42 * 10;;
(* - : int = 420 *)

3.14 /. 2.;;
(* - : float = 1.57 *)

4.2 ** 7.;;
(* - : float = 23053.9333248000075 *)

(* Exercise 3: equality *)

42 = 42;;
(* - : bool = true *)

"hi" = "hi";;
(* - : bool = true *)

"hi" == "hi";;
(* - : bool = false *)

(* Exercise 4: assert *)

assert true;;
(* - : unit = () *)

(* assert false;; Throws - Exception: Assert_failure *)

assert (2110 <> 3110);;
(* - : unit = () *)

(* Exercise 5: if *)

if 2 > 1 then 42 else 7;;
(* - : int = 42 *)

(* Exercise 6: double fun *)

let double x = x * 2;;

let _ = assert (double 7 = 14);;
let _ = assert (double 0 = 0);;
let _ = assert (double (-1) = -2);;

(* Exercise 7: more fun *)

let cube x = x ** 3.;; 

let _ = assert (cube 2. = 8.);;
let _ = assert (cube 0. = 0.);;
let _ = assert (cube (-3.) = -27.);;


let sign x = 
  if x > 0 then 1 
  else if x < 0 then -1 
  else 0;;

let _ = assert (sign 2 = 1);;
let _ = assert (sign (-5) = -1);;
let _ = assert (sign 0 = 0);;


let area r = Float.pi *. (r ** 2.);;

let _ = assert (area 2. = (Float.pi *. 4.));;
let _ = assert (area 5. = (Float.pi *. 25.));;
let _ = assert (area 1.2 = (Float.pi *. 1.44));;


(* Exercise 8: RMS *)

let rms x y = sqrt((x *. x +. y *. y) /. 2.)

let _ = assert (rms 3. 4. = sqrt(12.5));;

(* Exercise 9: date fun *)

let is_valid_date m d = 
  if m = "Jan" || m = "Mar" || m = "May" || m = "Jul" 
    || m = "Aug" || m = "Oct" || m = "Dec"
    then 0 < d && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov"
    then 0 < d && d <= 30
  else if m = "Feb"
    then 1 <= d && d <= 29
  else false;;

let _ = assert (is_valid_date "Mar" 15);;
(* 
   let _ = assert (is_valid_date "Jan" 32);; 
   Exception: Assert_failure 
*)

(* Exercise 10: fib *)

let rec fib n = 
  if n = 0
    then 0
  else if n = 1
    then 1
  else 
    fib(n - 1) + fib(n - 2);;

let _ = assert (fib 0 = 0);;
let _ = assert (fib 1 = 1);;
let _ = assert (fib 4 = 3);;


(* Exercise 11: fib fast *)

let rec h n pp p = 
  if n = 1 
    then p
  else
    h (n - 1) p (pp + p);;

let rec fib_fast n = 
  if n = 0 then 0
  else h n 0 1;;

  let _ = assert (fib_fast 0 = 0);;
  let _ = assert (fib_fast 1 = 1);;
  let _ = assert (fib_fast 50 = 12586269025);;


(* Exercise 12: poly types *)


let f x = if x then x else x
(* 
  bool -> bool as x must be bool to be
  used as conditional expression 
*)

let g x y = if y then x else x
(* 
  'a -> bool -> 'a as x is arbitrary type just returned. 
  But y needs to be bool to be used as conditional expression 
*)

let h x y z = if x then y else z
(* 
  bool -> 'a -> 'a - 'a x must be boolean to be a conditional 
  expression.contents y and z are generics, but as function 
  returns only one type, they are the same generic type.
*)

let i x y z = if x then y else y   
(* 
  bool -> a' -> b' -> a' x must be boolean to be conditional
  expression. y is generic type, z is also generic type but
  different one (never used).
*)

(* Exercise 13: divide *)

let divide ~numerator:n ~denominator:d = n /. d;;

let _ = assert (divide ~numerator:5. ~denominator:2. = 2.5);;
let _ = assert (divide ~numerator:6. ~denominator:2. = 3.);;


(* Exercise 14: associativity *)

let add x y = x + y;;

(*
  1. add 5 1 |+ legal common usage
  2. add 5 |+ partial application
  3. (add 5) 1 |+ applying second value to partially applied function
  4. add (5 1) |- (5 1) is not legal parentheses expresion add (5) (1) 
    is valid but redundant version of the example
*)

(* Exercise 15: average *)

let ( +/. ) x y = (x +. y) /. 2.;;

let _ = assert (1.0 +/. 2.0 = 1.5);;
let _ = assert (0. +/. 0. = 0.);;

(* Exercise 16: hello world *)

print_endline "Hello world!";;
print_string "Hello world!";;
(* 
  Hello world!
  - : unit = ()
  Hello world!- : unit = () 
*)
