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


