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
type tree = Tree of elem list
and elem = Num of int | Sub of tree

let rec square_tree (Tree lst) =
	let square_list = function
		| [] -> []
		| (Num i) :: t -> (Num (i * i)) :: t
		| (Sub tr) :: t -> (Sub (square_tree tr)) :: t
	in
		Tree (square_list lst)

let rec square_tree' (Tree lst) =
	let square_elem = function
		| Num i -> Num (i * i)
		| Sub tr -> Sub (square_tree' tr)
	in
		Tree (List.map square_elem lst)


(* A.3 *)
let rec tree_map f (Tree lst) =
	let f_elem = function
		| Num i -> f i
		| Sub tr -> Sub (tree_map f tr)
	in Tree (List.map f_elem lst)


(* A.4 *)
let rec subsets = function
	| [] -> [[]]
	| h :: t -> let rest = subsets t in
		rest @ (List.map (fun l -> h :: l) rest)
(*
The base case [] returns [[]], since the empty set only has itself as
its subset. If a set has one element i, it has the empty set [] and
itself as its subsets, and we can get a list of these items by combining
the subset of [] and i added to the subset of [], which is 
[] @ (i :: []). By inductive reasoning, this works for sets with more
than one element. For example, if we have a set [1; 2; 3], then we can
get its subsets by considering all subsets of the set [2; 3] (rest), and
the only new subsets that we have to consider are the subsets of set
[2; 3] with 1 added, which is (List.map (fun l -> h :: l) rest).
*)


(* A.5 *)
let rec accumulate op initial sequence = 
	match sequence with
		| [] -> initial
		| h :: t -> op h (accumulate op initial t)
		
let map p sequence =
	accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 = 
	accumulate (fun x r -> x :: r) seq2 seq1
	
let length sequence =
	accumulate (fun x r -> 1 + r) 0 sequence
	
	
(* A.6 *)
let rec accumulate_n op init seqs =
	match seqs with
		| [] -> failwith "empty list"
		| [] :: _ -> []   (* assume all sequences are empty *)
		| h :: t -> accumulate op init (List.hd h :: map List.hd t)
			:: accumulate_n op init (List.tl h :: map List.tl t)


(* A.7 *)

let rec map2 f x y =
	match (x, y) with
		| ([], []) -> []
		| ([], _) -> failwith "unequal lists"
		| (_, []) -> failwith "unequal lists"
		| ((xh :: xt), (yh :: yt)) ->
			f xh yh :: map2 f xt yt
			
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (dot_product v) m

let transpose mat = accumulate_n (fun x r -> x :: r) [] mat

let matrix_times_matrix m n =
	let cols = transpose n in
		map (matrix_times_vector cols) m
		

(* B.1 *)
