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
		| Num i -> Num (f i)
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
let rec quicksort lst cmp =
	match lst with
		| [] -> []
		| h :: t -> 
			let sm = List.filter (fun n -> cmp n h) t in
			let lg = List.filter (fun n -> not (cmp n h)) t in
				(quicksort sm cmp) @ (h :: (quicksort lg cmp))


(* B.2 *)
(*
The quicksort function uses generative recursion, as it generates parts
from the original list (list of elements smaller than the pivot and list
of elements equal to or larger than the pivot), recurses on each of
these, and combines results to construct the final result, rather than
recursing on the natural sub-part of list (rest) and combining with
first element.
*)


(* B.3 *)
(*
If the function merge_sort doesn't check for lists of length 1, when the
list has length 1 the function will split the list into eh = [] and
oh = itself, and merge_sort each half. When merge_sort oh cmp is called,
the function merge_sort will once again break oh into two parts ([] and
itself) and try to merge_sort each part, causing the function to enter
an infinite loop.
*)


(* B.4 *)
let rec insert_in_order new_result a_list cmp = 
	match a_list with 
		| [] -> [new_result]  
		| h :: t when cmp new_result h -> new_result :: a_list
		| h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
	match a_list with
		| [] -> []
		| h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* 
This represents structural recursion, as the function recurses on
"natural" subparts of data; the function insertion_sort recurses on tail
and combines the result with the head using insert_in_order, without
generating any new subparts to recurse on.
*)


(* C.1 *)
type expr =
	| Int of int
	| Var of string
	| Add of expr * expr
	| Mul of expr * expr
	| Pow of expr * int
	
let rec simplify1 expr =
	let rec pow a b =
		if b = 0
			then 1
			else a * (pow a (b - 1))
	in
		match expr with
			| Int _ 
			| Var _ -> expr
			| Add (Int i, Int j) -> Int (i + j)
			| Mul (Int i, Int j) -> Int (i * j)
			| Pow (Int i, j) -> Int (pow i j)
			| Add (Int 0, e)
			| Add (e, Int 0) 
			| Mul (Int 1, e)
			| Mul (e, Int 1)
			| Pow (e, 1) -> e
			| Mul (Int 0, _) 
			| Mul (_, Int 0) -> Int 0
			| Pow (e, 0) -> Int 1
			| Add (e1, e2)
			| Mul (e1, e2) -> Add (simplify1 e1, simplify1 e2)
			| Pow (e, i) -> Pow (simplify1 e, i)


(* C.2 *)
let rec deriv expr var = 
	match expr with
		| Int _ -> Int 0
		| Var v when v = var -> Int 1
		| Var _ -> Int 0
		| Add (e1, e2) -> Add (deriv e1 var, deriv e2 var)
		| Mul (e1, e2) -> 
			Add ((Mul (deriv e1 var, e2)), (Mul (e1, deriv e2 var)))
		| Pow (e, n) -> Mul (Int n, Mul (Pow (e, n - 1), deriv e var))
