## 2.9. Exercises

---

**Exercise: values \[★\]**

What is the type and value of each of the following OCaml expressions?

- `7 * (1 + 2 + 3)`
- `"CS " ^ string_of_int 3110`

_Hint: type each expression into the toplevel and it will tell you the answer. Note: `^` is not exponentiation._

---

**Exercise: operators \[★★\]**

Examine the [table of all operators in the OCaml manual](https://v2.ocaml.org/manual/expr.html#ss%3Aexpr-operators) (you will have to scroll down to find it on that page).

- Write an expression that multiplies `42` by `10`.
- Write an expression that divides `3.14` by `2.0`. _Hint: integer and floating-point operators are written differently in OCaml._
- Write an expression that computes `4.2` raised to the seventh power. _Note: there is no built-in integer exponentiation operator in OCaml (nor is there in C, by the way), in part because it is not an operation provided by most CPUs._

---

**Exercise: equality \[★\]**

- Write an expression that compares `42` to `42` using structural equality.
- Write an expression that compares `"hi"` to `"hi"` using structural equality. What is the result?
- Write an expression that compares `"hi"` to `"hi"` using physical equality. What is the result?

---

**Exercise: assert \[★\]**

- Enter `assert true;;` into utop and see what happens.
- Enter `assert false;;` into utop and see what happens.
- Write an expression that asserts 2110 is not (structurally) equal to 3110.

---

**Exercise: if \[★\]**

Write an if expression that evaluates to `42` if `2` is greater than `1` and otherwise evaluates to `7`.

---

**Exercise: double fun \[★\]**

Using the increment function from above as a guide, define a function `double` that multiplies its input by 2. For example, `double 7` would be `14`. Test your function by applying it to a few inputs. Turn those test cases into assertions.

---

**Exercise: more fun \[★★\]**

- Define a function that computes the cube of a floating-point number. Test your function by applying it to a few inputs.
- Define a function that computes the sign (1, 0, or -1) of an integer. Use a nested if expression. Test your function by applying it to a few inputs.
- Define a function that computes the area of a circle given its radius. Test your function with `assert`.

For the latter, bear in mind that floating-point arithmetic is not exact. Instead of asserting an exact value, you should assert that the result is “close enough”, e.g., within 1e-5. If that’s unfamiliar to you, it would be worthwhile to read up on [floating-point arithmetic](https://floating-point-gui.de/).

A function that take multiple inputs can be defined just by providing additional names for those inputs as part of the let definition. For example, the following function computes the average of three arguments:

```ocaml
let avg3 x y z = (x +. y +. z) /. 3.
```

---

**Exercise: RMS \[★★\]**

Define a function that computes the _root mean square_ of two numbers—i.e., $\sqrt{(x^2+y^2)/2}$. Test your function with `assert`.

---

**Exercise: date fun \[★★★\]**

Define a function that takes an integer `d` and string `m` as input and returns `true` just when `d` and `m` form a _valid date_. Here, a valid date has a month that is one of the following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number of days in that month, inclusive. For example, if the month is Jan, then the day is between 1 and 31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive.

How terse (i.e., few and short lines of code) can you make your function? You can definitely do this in fewer than 12 lines.

---

**Exercise: fib \[★★\]**

Define a recursive function `fib : int -> int`, such that `fib n` is the `n`th number in the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number), which is 1, 1, 2, 3, 5, 8, 13, … That is:

- `fib 1 = 1`,
- `fib 2 = 1`, and
- `fib n = fib (n-1) + fib (n-2)` for any `n > 2`.

Test your function in the toplevel.

---

**Exercise: fib fast \[★★★\]**

How quickly does your implementation of `fib` compute the 50th Fibonacci number? If it computes nearly instantaneously, congratulations! But the recursive solution most people come up with at first will seem to hang indefinitely. The problem is that the obvious solution computes subproblems repeatedly. For example, computing `fib 5` requires computing both `fib 3` and `fib 4`, and if those are computed separately, a lot of work (an exponential amount, in fact) is being redone.

Create a function `fib_fast` that requires only a linear amount of work. _Hint:_ write a recursive helper function `h : int -> int -> int -> int`, where `h n pp p` is defined as follows:

- `h 1 pp p = p`, and
- `h n pp p = h (n-1) p (pp+p)` for any `n > 1`.

The idea of `h` is that it assumes the previous two Fibonacci numbers were `pp` and `p`, then computes forward `n` more numbers. Hence, `fib n = h n 0 1` for any `n > 0`.

What is the first value of `n` for which `fib_fast n` is negative, indicating that integer overflow occurred?

---

**Exercise: poly types \[★★★\]**

What is the type of each of the functions below? You can ask the toplevel to check your answers.

```ocaml
let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y
```

---

**Exercise: divide \[★★\]**

Write a function `divide : numerator:float -> denominator:float -> float`. Apply your function.

---

**Exercise: associativity \[★★\]**

Suppose that we have defined `let add x y = x + y`. Which of the following produces an integer, which produces a function, and which produces an error? Decide on an answer, then check your answer in the toplevel.

- `add 5 1`
- `add 5`
- `(add 5) 1`
- `add (5 1)`

---

**Exercise: average \[★★\]**

Define an infix operator `+/.` to compute the average of two floating-point numbers. For example,

- `1.0 +/. 2.0 = 1.5`
- `0. +/. 0. = 0.`

---

**Exercise: hello world \[★\]**

Type the following in utop:

- `print_endline "Hello world!";;`
- `print_string "Hello world!";;`

Notice the difference in output from each.
