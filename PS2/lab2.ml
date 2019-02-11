open Num

let ni = num_of_int


(* A.1 *)
(*
The space compexity for this function is O(n). It is different from the
time complexity because the results of g^n operations don't all have to
be stored at the same time. OCaml evaluates each operand first before
applying the operator, meaning the the right operand doesn't get
evaluated (and thus expand to 2^n stacks) until the left operand is
fully evaluated. We can think of it as dfs rather than bfs; one path
is evaluated, the memory is returned, then another path is explored.
*)


(* A.2 *)
(* 

a. 5 times

b. The order of growth in space is O(log(a)), since the number of 
required recursive stacks is precisely ceil(log_3(10a)) + 1. The order
of growth in number of steps is also O(log(a)), because the function is
called ceil(log_3(10a)) + 1 times and each function call performs a
constant amount of work (including calling other constant functions.

*)


(* A.3.1 *)
let rec fast_expt b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in 
		match n with
			| 0 -> 1
			| n' when is_even n' -> square (fast_expt b (n' / 2))
			| _ -> b * fast_expt b (n - 1)
			
(* A.3.2 *)
let ifast_expt b n =
	let is_even m = m mod 2 = 0 in
	let square m = m * m in
	let rec iter a b n = 
		match n with
			| 0 -> a
			| n' when is_even n' -> iter a (square b) (n' / 2)
			| _ -> iter (a * b) b (n - 1)
	in
		iter 1 b n


(* A.4 *)
let rec fast_mult a b =
	let is_even n = n mod 2 = 0 in
	let double n = n * 2 in
	let halve n = n / 2 in
		match b with
			| 0 -> 0
			| b' when is_even b' -> double (fast_mult a (halve b'))
			| _ -> a + fast_mult a (b - 1)
		
	
(* A.5 *)
let ifast_mult a b =
	let is_even n = n mod 2 = 0 in
	let double n = n * 2 in
	let halve n = n / 2 in
	let rec iter c a b =
		match b with
			| 0 -> c
			| b' when is_even b' -> iter c (double a) (halve b')
			| _ -> iter (c + a) a (b - 1)
	in
		iter 0 a b
				
		
(* A.6 *)
(*
The time complexity is O(n) and space complexity is O(log(n)). The
function is a tree recursive process with log(n) depth. Thus the space
complexity corresponds to the maximum number of recursive stacks, the
tree depth, and the time complexity corresponds to the number of
function calls, which is the number of nodes in the tree 2^(log_2(n)
= n.
*)


(* A.7 *)
(*

1. This function is a linear recursive process, as each call makes one
recursive call such taht the recursive function is called a total of
n + 1 times.

2. The space and time complexities of this function are both O(n), since
there will be n + 1 recursive calls with a maximum of n pending
operations.

*)
	
	
(* B.1 *)
(*

a. 
(fun x y -> x * (2 + y)) 20 (2 * 4)

b.
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0

c.
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1

d.
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1

*)


(* B.2 *)
(*

Desugar:
-> (fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
-> (fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
-> (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
Evaluate fun x y -> ...
  evaluate (2 * 10) -> 20
  evaluate (3 + 4) -> 7
  apply fun x y -> ... to 20, 7
    substitute 20 for x, 7 for y in body (y shielded)
    -> (fun y -> (fun z -> 20 * y * z) 22) 14
    evaluate:
      apply fun y -> ... to 14
        substitute 14 for y in body
        -> (fun z -> 20 * 14 * z) 22
        evaluate:
          apply fun z -> ... to 22
            substitute 22 for z in body
            -> 20 * 14 * 22
            evaluate 20 * 14 * 22 -> 6160
  Result: 6160

*)


(* B.3 *)
(*

Desugared:
(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

Ben's code doesn't work because all the operands 10, (x * 2), and
(y + 3) are first evaluated then x, y, and z are bound to these
expressions for evaluating the function body. When (x * 2) is being
evaluated, x hasn't been bound to 10 yet. Using nested lets would fix
this problem:

let x = 10 in
let y = x * 10 in
let z = y + 3 in
	x + y + z

*)


(* C.1 *)
let isum term a next b = 
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (result +/ (term a))
	in
		iter a (ni 0)


(* C.2 *)
let rec product_rec term a next b =
	if a >/ b
		then (ni 1)
		else term a */ (product_rec term (next a) next b)
		
let factorial_rec n =
	let step1 m = m +/ (ni 1) in
		if n <=/ (ni 1)
			then (ni 1)
			else product_rec (fun m -> m) (ni 1) step1 n
			
let pi_product n =
	let term_num m = m +/ (ni 2) -/ (mod_num m (ni 2)) in
	let term_den m = m +/ (ni 2) -/ (mod_num (m +/ (ni 1)) (ni 2)) in
	let skip1 m = m +/ (ni 1) in
	let num = product_rec term_num (ni 1) skip1 n in
	let den = product_rec term_den (ni 1) skip1 n in
		(ni 4) */ num // den

let pi_approx = float_of_num (pi_product (ni 5000))

let product_iter term a next b =
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (result */ (term a))
	in
		iter a (ni 1)

let factorial_iter n =
	let step1 m = m +/ (ni 1) in
		if n <=/ (ni 1)
			then (ni 1)
			else product_iter (fun m -> m) (ni 1) step1 n	


(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
	if a >/ b
		then null_value
		else combiner (term a) 
			(accumulate_rec combiner null_value term (next a) next b)
			
let accumulate_iter combiner null_value term a next b =
	let rec iter a result =
		if a >/ b
			then result
			else iter (next a) (combiner result (term a))
	in
		iter a null_value

let sum term a next b =
	accumulate_iter (+/) (ni 0) term a next b

let product term a next b =
	accumulate_iter ( */ ) (ni 1) term a next b


(* C.4 *)
let compose f g =
	fun x -> f (g x)
	
	
(* C.5 *)
let rec repeated f n =
	if n = 0
		then fun x -> x
		else compose f (repeated f (n - 1))


(* C.6 *)
let smooth dx f =
	fun x -> (f (x -. dx) +. f x +. f (x +. dx)) /. 3.0


(* C.7 *)
let nsmoothed dx f n =
	(repeated (smooth dx) n) f


(* D.1 *)
let is_prime n =
	let sqrt_n = int_of_float (sqrt (float_of_int n)) in
	let rec iter i = 
		if i > sqrt_n
			then true
			else if n mod i = 0
				then false
				else iter (i + 1)
	in
		if n < 2
			then false
			else iter 2


(* D.2 *)
let smallest_prime_factor n =
	let rec iter i =
		if n mod i = 0 && is_prime i
			then i
			else iter (i + 1)
	in
		if is_prime n || n < 2
			then invalid_arg 
				"Argument should be a composite number greater than 2"
			else iter 2
