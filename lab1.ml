(* A.1 *)

(*

1. - : int = 10

2. - : float = 10.

3. - : int = 12

4. Error: This expression has type float but an expression was expected 
of type int
- This error occurred because + is for integer addition but 3.2 and 4.2
are floats.

5. Error: This expression has type int but an expression was expected of
type float
- This error occurred because +. is for float addition but 3 and 4 are 
integers.

6. Error: This expression has type float but an expression was expected
of type int
- This error occurred because + is for integer addition but 4.2 is a
float.

7. Error: This expression has type int but an expression was expected of
type float
- This error occurred because +. is for float addition but 3 is an
integer.

8. - : float = 7.2

9. - : int = 5

10. - : int = 7

11. val a : int = 3

12. val b : int = 4

13. - : bool = false

14. - : bool = true

15. - : bool = false
- It's different from the previous expression because = means
"structurally equal" while == means "identical".

16. - : (int * int * int) list = [(1, 2, 3)]

17. - : (int * int * int) list = [(1, 2, 3)]
- List elements are separated by semicolons, while tuple elements are
separated by commas. Using commas as separators means that the elements
are tuple elements.

18. - : int = 4

19. Error: Syntax error
- The and operator in OCaml is &&, not "and".

20. - : int = 6

21. Error: This expression has type int but an expression was expected
of type unit because it is in the result of a conditional with no else
branch
- This gives a type error because OCaml assumes an implicit else () of
type unit, and then and else clause expressions must have the same type,
but the then clause in this expression has type int.

*)


(* A.2 *)
let sum_of_squares_of_two_largest x y z = 
    if x < y && x < z
        then y * y + z * z
        else if y > z
                then x * x + y * y
                else x * x + z * z
                

(* A.3 *)
(*
The conditional expression is evaluated as (+) if b > 0 and (-) if 
b <= 0. Thus the function will return a + b if b > 0 and a - b if
b <= 0, which is equivalent to adding the absolute value of b to a.
*)


(* B.1 *)


