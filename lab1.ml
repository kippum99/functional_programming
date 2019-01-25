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
(*
With applicative-order evaluation, Ben will observe an infinite loop
because the function arguments 0 and p () are evaluated before the
function is called, and the recursion function p () has no terminating
condition. With normal-order evaluation, he will get 0 as a result
because the function test 0 (p ()) returns 0 as soon as it evaluates
0 = 0 to be true.
*)


(* B.2 *)
(*
The sqrt_iter function will enter an infinite loop, because the function
arguments of new_if are evaluated before the function is called, so 
whether the predicate (is_good_enough guess x) is true or false
the else clause (sqrt_iter (improve guess x) x) will always be
evaluated. Thus sqrt_iter will keep calling itself without a terminating
condition.
*)


(* B.3 *)
(*
Evaluate: add_a 2 5
    evaluate 2 -> 2
    evaluate 5 -> 5
    evaluate add_a -> desugar to:
        fun a b -> ...
    apply fun a b -> ... to 2, 5
        substitute 2 for a, 5 for b in body:
            if 2 = 0 then 5 else inc (add_a (dec 2) 5)
        evaluate if 2 = 0 then 5 else inc (add_a (dec 2) 5)
			*special form rule for if: evaluate predicate first
            evaluate 2 = 0
                evaluate 2 -> 2
                evaluate 0 -> 0
                evaluate = -> =
                apply = to 2, 0 -> false
            * special form rule for if: since predicate is false, replace with false clause:
            evaluate inc (add_a (dec 2) 5)
                evaluate add_a (dec 2) 5
                    evaluate dec 2
                        evaluate 2 -> 2
                        evaluate dec -> dec
                        apply dec to 2 -> 1
                    evaluate 5 -> 5
                    evaluate add_a -> desugar to:
                        fun a b -> ...
                    apply fun a b -> ... to 1, 5
                        substitute 1 for a, 5 for b in body:
                            if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                        evaluate if 1 = 0 then 5 else inc (add_a (dec 1) 5)
							*special form rule for if: evaluate predicate first
                            evaluate 1 = 0
                                evaluate 2 -> 2
                                evaluate 0 -> 0
                                evaluate = -> =
                                apply = to 2, 0 -> false
                            * special form rule for if: since predicate is false, replace with false clause:
                            evaluate inc (add_a (dec 1) 5)
                                evaluate add_a (dec 1) 5
                                    evaluate dec 1
                                        evaluate 1 -> 1
                                        evaluate dec -> dec
                                        apply dec to 1 -> 0
                                    evaluate 5 -> 5
                                    evaluate add_a -> desugar to:
                                        fun a b -> ...
                                    apply fun a b -> ... to 0, 5
                                        substitute 0 for a, 5 for b in body:
                                            if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                        evaluate if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                            *special form rule for if: evaluate predicate first
                                            evaluate 0 = 0
                                                evaluate 0 -> 0
                                                evaluate 0 -> 0
                                                evaluate = -> 0
                                                apply = to 0, 0 -> true
                                            * special form rule for if: since predicate is true, replace with true clause:
                                            evaluate 5 -> 5
                                evaluate inc -> inc
                                apply inc to 5 -> 6
				evaluate inc -> inc
				apply inc to 6 -> 7
	Result: 7
	
Evaluate: add_b 2 5
	evaluate 2 -> 2
	evaluate 5 -> 5
	evaluate add_b -> desugar to:
		fun a b -> ...
	apply fun a b -> ... to 2, 5
		substitute 2 for a, 5 for b in body:
			if 2 = 0 then 5 else add_b (dec 2) (inc 5)
		evaluate if 2 = 0 then 5 else add_b (dec 2) (inc 5)
			*special form rule for if: evaluate predicate first
			evaluate 2 = 0
				evaluate 2 -> 2
				evaluate 0 -> 0
				evaluate = -> =
				apply = to 2, 0 -> false
			*special form rule for if: since predicate is false, replace with false clause:
			evaluate add_b (dec 2) (inc 5)
				evaluate dec 2
					evaluate 2 -> 2
					evaluate dec -> dec
					apply dec to 2 -> 1
				evaluate inc 5
					evaluate 5 -> 5
					evaluate inc -> inc
					apply inc to 5 -> 6
				evaluate add_b -> desugar to:
					fun a b -> ...
				apply fun a b -> ... to 1, 6
					substitute 1 for a, 6 for b in body:
						if 1 = 0 then 6 else add_b (dec 1) (inc 6)
					evaluate if 1 = 0 then 6 else add_b (dec 1) (inc 6)
						*special form rule for if: evaluate predicate first
						evaluate 1 = 0
							evaluate 1 -> 1
							evaluate 0 -> 0
							evaluate = -> =
							apply = to 1, 0 -> false
						*special form rule for if: since predicate is false, replace with false clause:
						evaluate add_b (dec 1) (inc 6)
							evaluate dec 1
								evaluate 1 -> 1
								evaluate dec -> dec
								apply dec to 1 -> 0
							evaluate inc 6
								evaluate 6 -> 6
								evaluate inc -> inc
								apply inc to 6 -> 7
							evaluate add_b -> desugar to:
								fun a b -> ...
							apply fun a b -> ... to 0, 7
								substitute 0 for a, 7 for b in body:
									if 0 = 0 then 7 else add_b (dec 0) (inc 7)
								evaluate if 0 = 0 then 7 else add_b (dec 0) (inc 7)
								*special form rule for if: evaluate predicate first
								evaluate 0 = 0
									evaluate 0 -> 0
									evaluate 0 -> 0
									evaluate = -> =
									apply = to 0, 0 -> true
								*special form rule for if: since predicate is true, replace with true clause:
								evaluate 7 -> 7
	Result: 7
	
add_a is recursive, while add_b is iterative (due to tail recursion optimization).
                                    
*)


(* C.1 *)

(* This function computes the factorial of the input number,
 which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
	if n = 0 then 1 else n * factorial (n - 1)
	
(* C.1.a *)
let e_term i = 
	1. /. float_of_int (factorial i)

(* C.1.b *)
let rec e_approximation n =
	if n = 0
		then e_term 0
		else e_term n +. e_approximation (n - 1)

(* C.1.c *)
(*
e_approximation 20 gives 2.71828182845904553, and exp 1.0 gives
2.71828182845904509, which are the same up to the e-15 digit.
*)

(* C.1.d *)
(*
It returns infinity because large factorial values result in integer
overflow, which defaults to 0 and sets the e_term values to infinity.
*)


(* C.2 *)
let rec is_even n =
	if n = 0 then true else is_odd (n - 1)
and is_odd n =
	if n = 0 then false else is_even (n - 1)


(* C.3 *)
let rec f_rec n = 
	if n < 3
		then n
		else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3)

let f_iter n =
	let rec iter a b c cnt n =
		if cnt > n
			then a
			else iter (a + 2 * b + 3 * c) a b (cnt + 1) n
	in
		if n < 3
			then n
			else iter 2 1 0 3 n


(* C.4 *)
let rec pascal_coefficient i j =
	match (i, j) with
		| (i', j') when i' < 1 || j' < 1 -> failwith "invalid arguments"
		| (i', j') when j' > i' -> failwith "invalid arguents"
		| (i', j') when j' = i' -> 1
		| (_, 1) -> 1
		| (_, _) -> pascal_coefficient (i - 1) (j - 1) + pascal_coefficient (i - 1) j

