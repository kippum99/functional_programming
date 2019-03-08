(* B.1 *)

exception Stat_error of string

let make_stat_1 () =
	let sum = ref 0. in
	let sumsq = ref 0. in
	let n = ref 0 in
		object
			method append x = sum := !sum +. x
			method clear = sum := 0.
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
			method append x = sum := !sum +. x
			method clear = sum := 0.
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
		let find_rank = function
			| Leaf -> 0
			| Node (_, r, _, _) -> r
		let make_new min q1 q2 =
			let r1 = find_rank q1 in
			let r2 = find_rank q2 in
				if r1 < r2 then
					Node (min, r1 + 1, q2, q1)
				else
					Node (min, r2 + 1, q1, q2)
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
		let find_rank = function
			| Leaf -> 0
			| Node (_, r, _, _) -> r
		let make_new min q1 q2 =
			let r1 = find_rank q1 in
			let r2 = find_rank q2 in
				if r1 < r2 then
					Node (min, r1 + 1, q2, q1)
				else
					Node (min, r2 + 1, q1, q2)
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
