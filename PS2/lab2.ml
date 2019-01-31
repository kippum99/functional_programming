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
