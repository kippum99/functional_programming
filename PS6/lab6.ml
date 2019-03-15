(* A.1 *)
(*

FRAME 0 (initial environment)
	parent: none
	bindings:
		- : [primitive function -]
		* : [primitive function *]
		
FUNCTION 0 (fun n -> let rec iter ...)
	env: FRAME 0
	param: n
	body: let rec iter m r = ...
	
FRAME 1 (let factorial = FUNCTION 0 in ...)
	parent: FRAME 0
	bindings:
		factorial : FUNCTION 0
	
FRAME 2 (FUNCTION 0 applied to 3)
	parent: FRAME 0
	bindings:
		n : 3
	
FRAME 3 (let rec iter = FUNCTION 1 in iter n 1)
	parent: FRAME 2
	bindings:
		iter : FUNCTION 1
		
FUNCTION 1 (fun m r -> if m = 0 then ...)
	env: FRAME 3
	param: m, r
	body: if m = 0 then ...
		
FRAME 4 (FUNCTION 1 applied to 3, 1)
	parent: FRAME 3
	bindings: 
		m : 3
		r : 1
		
FRAME 5 (FUNCTION 1 applied to 2, 3)
	parent: FRAME 3
	bindings:
		m : 2
		r : 3
		
FRAME 6 (FUNCTION 1 applied to 1, 6)
	parent: FRAME 3
	bindings:
		m : 1
		r : 6
		
FRAME 7 (FUNCTION 1 applied to 0, 6)
	parent: FRAME 3
	bindings:
		m : 0
		r : 6

*)


(* A.2 *)
let factorial = 
	let f = ref (fun n -> 0) in
		begin
			f := function
				| 0 -> 1
				| n' -> n' * !f (n' - 1);
		end;
		!f

(* B.1 *)

exception Stat_error of string

let make_stat_1 () =
	let sum = ref 0. in
	let sumsq = ref 0. in
	let n = ref 0 in
		object
			method append x = 
				sum := !sum +. x;
				sumsq := !sumsq +. x *. x;
				n := !n + 1
			method clear = 
				sum := 0.;
				sumsq := 0.;
				n := 0
			method mean = 
				if !n = 0 then
					raise 
						(Stat_error "need at least one value for mean")
				else !sum /. (float_of_int !n)
			method stdev = 
				let n' = float_of_int !n in 
					if !n = 0 then
						raise (Stat_error 
							"need at least one value for stdev")
					else sqrt ((!sumsq -. !sum *. !sum /. n') /. n')
			method variance =
				let n' = float_of_int !n in 
					if !n = 0 then
						raise (Stat_error 
							"need at least one value for variance")
					else (!sumsq -. !sum *. !sum /. n') /. n'
		end


(* B.2 *)

let make_stat_2 () =
	let sum = ref 0. in
	let sumsq = ref 0. in
	let n = ref 0 in
		object (self)
			method append x =
				sum := !sum +. x;
				sumsq := !sumsq +. x *. x;
				n := !n + 1
			method clear = 
				sum := 0.;
				sumsq := 0.;
				n := 0
			method mean = 
				if !n = 0 then
					raise 
						(Stat_error "need at least one value for mean")
				else !sum /. (float_of_int !n)
			method private _variance = 
				let n' = float_of_int !n in
					(!sumsq -. !sum *. !sum /. n') /. n'
			method stdev = 
				if !n = 0 then
					raise 
						(Stat_error "need at least one value for stdev")
				else sqrt self#_variance
			method variance =
				if !n = 0 then
					raise (Stat_error 
							"need at least one value for variance")
				else self#_variance
		end


(* C.1 *)

module type PRIORITY_QUEUE =
  sig
    exception Empty
    type elem
    type t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end
  
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
	struct
		exception Empty 
		type elem = int
		type t = Leaf | Node of elem * int * t * t
		let empty = Leaf
		let is_empty q = q = Leaf
		let find_min = function
			| Leaf -> raise Empty
			| Node (el, _, _, _) -> el
		(* Helper function that returns rank of the node *)
		let find_rank = function
			| Leaf -> 0
			| Node (_, r, _, _) -> r
		(* Helper function that returns a new heap made from a minimum
		 * element and two heaps *)
		let make_new min q1 q2 =
			let r1 = find_rank q1 in
			let r2 = find_rank q2 in
				if r1 < r2 then
					Node (min, r1 + 1, q2, q1)
				else
					Node (min, r2 + 1, q1, q2)
		(* Helper function that merges two leftist heaps and returns a 
		 * new leftist heap *)
		let rec merge q1 q2 =
			match (q1, q2) with
				| (Leaf, q2') -> q2'
				| (q1', Leaf) -> q1'
				| (Node (m1, _, l1, r1), q2') when m1 < find_min q2' ->
					make_new m1 l1 (merge r1 q2')
				| (q1', Node (m2, _, l2, r2)) ->
					make_new m2 l2 (merge r2 q1')
		let insert q el = merge q (Node (el, 1, Leaf, Leaf))
		let delete_min = function
			| Leaf -> raise Empty
			| Node (_, _, l, r) -> merge l r
		let rec from_list = function
			| [] -> empty
			| h :: t -> insert (from_list t) h
	end
	
let heap_sort lst =
	let rec iter queue rev_lst =
		if PriorityQueue.is_empty queue then
			rev_lst
		else 
			iter (PriorityQueue.delete_min queue) 
				(PriorityQueue.find_min queue :: rev_lst)
	in
		List.rev (iter (PriorityQueue.from_list lst) [])
	
	
(* C.2 *)
type comparison = LT | EQ | GT

module type ORDERED = sig type t val cmp : t -> t -> comparison end

module MakePriorityQueue(Elt: ORDERED) 
	: (PRIORITY_QUEUE with type elem = Elt.t) =
	struct 
		exception Empty 
		type elem = Elt.t
		type t = Leaf | Node of elem * int * t * t
		let empty = Leaf
		let is_empty q = q = Leaf
		let find_min = function
			| Leaf -> raise Empty
			| Node (el, _, _, _) -> el
		(* Helper function that returns rank of the node *)
		let find_rank = function
			| Leaf -> 0
			| Node (_, r, _, _) -> r
		(* Helper function that returns a new heap made from a minimum
		 * element and two heaps *)
		let make_new min q1 q2 =
			let r1 = find_rank q1 in
			let r2 = find_rank q2 in
				if r1 < r2 then
					Node (min, r1 + 1, q2, q1)
				else
					Node (min, r2 + 1, q1, q2)
		(* Helper function that merges two leftist heaps and returns a 
		 * new leftist heap *)
		let rec merge q1 q2 =
			match (q1, q2) with
				| (Leaf, q2') -> q2'
				| (q1', Leaf) -> q1'
				| (Node (m1, _, l1, r1), q2') 
					when Elt.cmp m1 (find_min q2') = LT ->
					make_new m1 l1 (merge r1 q2')
				| (q1', Node (m2, _, l2, r2)) ->
					make_new m2 l2 (merge r2 q1')
		let insert q el = merge q (Node (el, 1, Leaf, Leaf))
		let delete_min = function
			| Leaf -> raise Empty
			| Node (_, _, l, r) -> merge l r
		let rec from_list = function
			| [] -> empty
			| h :: t -> insert (from_list t) h		
	end

module OrderedString =
	struct
		type t = string
		let cmp x y = 
			if x = y then EQ else if x < y then LT else GT
	end
	
module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
	let rec iter queue rev_lst =
		if StringPQ.is_empty queue then
			rev_lst
		else 
			iter (StringPQ.delete_min queue) 
				(StringPQ.find_min queue :: rev_lst)
	in
		List.rev (iter (StringPQ.from_list lst) [])	
	
	
(* D *)

type 'a lazy_store =
	| Expr of (unit -> 'a)
	| Val of 'a
	
type 'a lazy_t = 'a lazy_store ref 
	
let make_lazy e = ref (Expr e)

let force lz =
	match !lz with
		| Val x -> x
		| Expr e -> 
			let x = e () in
				lz := Val x;
				x

(* D.1 *)	

let almost_sum =
	fun f ->
		fun lst ->
			match lst with
				| [] -> 0
				| h :: t -> h + f t
				
let y = 
	fun f -> 
		(fun z -> z (`Roll z)) 
		(fun (`Roll w) -> f (fun x -> w (`Roll w) x))
		
let sum = y almost_sum 


(* D.2 *)

let almost_iter =
	fun f ->
		fun (n, r) ->
			if n = 0
				then r
				else f (n - 1, n * r)

let factorial2 n = y almost_iter (n, 1)
