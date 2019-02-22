(* name: Joo Eun (June) Kim *)
(* login: kooeun *)


(* 1.a. *)
(*
Time complexity: O(log n)
This recursive function has 0 as its base case and calls itself
if the argument n is greater than 0, by doing a floor division by 2 and
passing it in as the argument. Each function call does a constant
amount of work. The time complexity scales with precisely
floor(log_2 n) + constant, giving us O(log n).
*)


(* 1.b. *)
(*
Time complexity: O(n)
This recursive function has 0 as its base case and calls itself if the
argument n is greater than 0. Worst-case time complexity is when n is
odd, since it passes in n - 2, another odd number, as an argument to the
recursive call. Each recursive call does a constant amount of work, so
the time complexity is n / 2 for odd n and log n for even n, giving us 
O(n) as the worst case time complexity.
*)


(* 1.c *)
(*
Time complexity: O((log n)^2)
The recursive function iter gets called (log_2 n) * (1 + log_2 n) + 1
times and does a constant amount of work each time. The end condition 
for the recursion is a >= n, and a gets multiplied by 2 when b >= n. It 
takes 1 + log_2 n calls for b to get from 1 to n, and since b resets to 
1 when a is multiplied by 2, it takes a (log_2 n) * (1 + log_2 n) calls 
to get from 1 to n. The term log_2 n * log_2 n dominates the asymptotic 
time complexity, giving us O((log n)^2).
*)


(* 1.d *)
(*
Time complexity: O(n * log n)
Let time of merge_sort3 = T(n) where n is the length of the input list.
Splitting in three lists has time complexity O(n) = c1 * N.
Sorting the three lists takes 3 * T(n / 3), and merging in order takes 
O(n) = c2 * n. Letting c = c1 + c2, each call of merge_sort3 takes 
T(n) = c * n + 3 * T (n / 3), and base case T(1) = O(1).
T(n) = c * n + 3 * c * n / 3 + 9 * c * n / 9 + ...
	 = c * n + c * n + c * n + ...
Since we split lists log_3 n times and we do O(n) work for each split,
the overall time complexity is O(n * log n).
*)


(* 2.1 *)
let split3 lst =
	let rec scan lst' i =
		match lst' with
			| [] -> ([], [], [])
			| h :: t ->
				let (lst1, lst2, lst3) = scan t (i + 1) in
					match () with
						| _ when i mod 3 = 0 -> (h :: lst1, lst2, lst3)
						| _ when i mod 3 = 1 -> (lst1, h :: lst2, lst3)
						| _ -> (lst1, lst2, h :: lst3)
	in
		scan lst 0

let merge3 lst1 lst2 lst3 =
	let rec merge2 lst1' lst2' =
		match (lst1', lst2') with
			| ([], _) -> lst2'
			| (_, []) -> lst1'
			| (h1 :: t1, h2 :: _) when h1 < h2 -> h1 :: merge2 t1 lst2'
			| (_, h2 :: t2) -> h2 :: merge2 lst1' t2
	in
		merge2 (merge2 lst1 lst2) lst3
				
			
(* 2.2.a *)
let smallest_index lst =
	let rec iter lst' min min_i i =
		match lst' with
			| [] -> min_i
			| h :: t ->
				if h < min 
					then iter t h i (i + 1)
					else iter t min min_i (i + 1)
	in
		match lst with
			| [] -> invalid_arg "smallest_index: not enough elements"
			| h :: t -> iter t h 0 1
			
(* 2.2.b *)
let flip_n n lst =
	let rec iter lst' flipped i =
		if i = n
			then flipped @ lst'
			else 
				match lst' with
					| [] -> invalid_arg "flip_n: not enough elements"
					| h :: t -> iter t (h :: flipped) (i + 1)
	in
		iter lst [] 0
		
(* 2.2.c *)
let block_sort1 lst =
	match lst with 
		| [] 
		| [_] -> lst
		| _ -> flip_n (smallest_index lst + 1) lst
	
(* 2.2.d *)

(*
The function block_sort uses structural recursion, as it recurses on
"natural" subparts on the data; the function recurses on the tail and
combines the result with the head, without generating any new subparts
to recurse on.
*)

let block_sorti lst =
	let rec iter lst' sorted =
		match block_sort1 lst' with
			| [] -> sorted
			| h :: t -> iter t (sorted @ [h])
	in
		iter lst []


(* 3.1.a *)
let linrec is_base on_base splitter combine =
	let rec f x =
		if is_base x then
			on_base x
		else
			let (a, b) = splitter x in
				combine a (f b)
	in f

(* 3.1.b *)
let insert_r item =
	let is_base lst = lst = [] || item <= List.hd lst in
	let on_base lst = item :: lst in
	let splitter lst = (List.hd lst, List.tl lst) in
	let combine first rest_after_rec = first :: rest_after_rec in
		linrec is_base on_base splitter combine

(* 3.1.c *)
let insertion_sort =
	let is_base lst = lst = [] in
	let on_base _ = [] in
	let splitter lst = (List.hd lst, List.tl lst) in
	let combine first rest_after_rec = insert_r first rest_after_rec in
		linrec is_base on_base splitter combine


(* 3.2.a *)
let binrec is_base on_base splitter combine =
	let rec f x =
		if is_base x then
			on_base x
		else
			let (a, b, c) = splitter x in
				combine a (f b) (f c)
	in f
	
(* 3.2.b *)
let quicksort =
	let is_base lst = lst = [] in
	let on_base _ = [] in
	let splitter lst =
		match lst with
			| [] -> invalid_arg "quicksort: can't split"
			| h :: t ->
				let is_lt x = x < h in
				let is_ge x = x >= h in
					(h, List.filter is_lt t, List.filter is_ge t)
	in
	let combine pivot lt ge = lt @ (pivot :: ge) in
		binrec is_base on_base splitter combine


(* 3.3.a *)
let tailrec is_base on_base next =
	let rec f inputs =
		if is_base inputs then
			on_base inputs
		else f (next inputs)
	in f

(* 3.3.b *)
let insert_i item lst =
	let is_base (_, rest) = rest = [] || item <= List.hd rest in
	let on_base (prev, rest) = prev @ (item :: rest) in
	let next (prev, rest) = (prev @ [List.hd rest], List.tl rest) in
	let iter = tailrec is_base on_base next in
		iter ([], lst)
		
(* 3.3.c *)
let insertion_sort_i lst =
	let is_base (_, rest) = rest = [] in
	let on_base (prev, _) = prev in
	let next (prev, rest) = 
		(insert_i (List.hd rest) prev, List.tl rest)
	in
	let iter = tailrec is_base on_base next in
		iter ([], lst)


(* 4.1 *)
type tree = 
	| Leaf 
	| Node of int * int * tree * tree
	
let rec member i tr =
	match tr with
		| Leaf -> false
		| Node (lvl, v, l, r) ->
			if i = v then
				true
			else 
				if i < v then
					member i l
				else member i r
				
				
(* 4.2 *)
let skew tr =
	match tr with
		| Node (lvl, v, Node (llvl, lv, ll, lr), r) when lvl = llvl ->
			Node (lvl, lv, ll, Node (lvl, v, lr, r))
		| _ -> tr

let level = function
	| Leaf -> 0
	| Node (lvl, _, _, _) -> lvl
	
let split tr =
	match tr with
		| Node (lvl, v, l, Node (lvl2, v2, l2, r2))
			when (lvl = lvl2) && (lvl = level r2) ->
				Node (lvl + 1, v2, Node (lvl, v, l, l2), r2)
		| _ -> tr


(* 4.3 *)
let rec insert item t =
	match t with
		| Leaf -> Node (1, item, Leaf, Leaf)
		| Node (_, v, _, _) when item = v -> t
		| Node (lvl, v, l, r) when item < v ->
			split (skew (Node (lvl, v, insert item l, r)))
		| Node (lvl, v, l, r) ->
			split (skew (Node (lvl, v, l, insert item r)))
