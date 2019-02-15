(* A.1 *)
type mobile = Mobile of branch * branch
and branch = Weight of int * int | Structure of int * mobile

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* A.1.a *)
let left_branch (Mobile (l, _)) = l

let right_branch (Mobile (_, r)) = r

let branch_length = function
	| Weight (l, _) -> l
	| Structure (l, _) -> l

let branch_structure = function
	| Weight (_, w) -> `Weight w
	| Structure (_, m) -> `Structure m

(* A.1.b *)
let rec branch_weight1 = function
	| Weight (_, w) -> w
	| Structure (_, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) =
	(branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b =
	match branch_structure b with
		| `Weight w -> w
		| `Structure m -> total_weight2 m
and total_weight2 m =
	(branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))
	
(* A.1.c *)
let rec is_balanced m =
	let is_balanced_branch b =
		match branch_structure b with
			| `Weight w -> true
			| `Structure m -> is_balanced m
	in
	let l = left_branch m in
	let r = right_branch m in
		if (branch_length l) * (branch_weight2 l) <>
			(branch_length r) * (branch_weight2 r)
			then false
			else (is_balanced_branch l) && (is_balanced_branch r)
			
(* A.1.d *)
type mobile' = { left : branch'; right : branch'; }
and branch' = Branch' of int * contents
and contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = { left; right }
let make_weight' l w = Branch' (l, (Weight' w))
let make_structure' l m = Branch' (l, (Structure' m))

let left_branch' { left } = left
let right_branch' { right } = right

let branch_length' (Branch' (l, _)) = l

let branch_structure' (Branch' (_, c)) =
	match c with 
		| Weight' w -> `Weight w
		| Structure' m -> `Structure m

let rec branch_weight' b =
	match branch_structure' b with
		| `Weight w -> w
		| `Structure m -> total_weight' m
and total_weight' m =
	(branch_weight' (left_branch' m)) 
		+ (branch_weight' (right_branch' m))
		
let rec is_balanced' m =
	let is_balanced_branch b =
		match branch_structure' b with
			| `Weight w -> true
			| `Structure m -> is_balanced' m
	in
	let l = left_branch' m in
	let r = right_branch' m in
		if (branch_length' l) * (branch_weight' l) <>
			(branch_length' r) * (branch_weight' r)
			then false
			else (is_balanced_branch l) && (is_balanced_branch r)


(* A.2 *)

