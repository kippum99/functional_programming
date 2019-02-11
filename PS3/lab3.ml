(* A.1 *)

type point = { x : float; y : float }
type segment = { startp : point; endp : point }

let make_point x y = { x; y }
let get_coords { x; y } = (x, y)

let make_segment startp endp = { startp; endp }
let get_points { startp; endp } = (startp, endp)

let midpoint_segment { startp; endp } = 
	let x = (startp.x +. endp.x) /. 2. in
	let y = (startp.y +. endp.y) /. 2. in
		{ x; y }

let segment_length { startp; endp } =
	let lenx = abs_float (endp.x -. startp.x) in
	let leny = abs_float (endp.y -. startp.y) in
		sqrt (lenx *. lenx +. leny *. leny)

let print_point { x; y} =
	Printf.printf "(%g, %g)" x y


(* A.2 *)

type rectangle = { ll : point; ur : point }

let rectangle_lower_segment { ll; ur } =
	let startp = ll in
	let endp = { x = ur.x; y = ll.y } in
		{ startp; endp }

let rectangle_upper_segment { ll; ur } =
	let startp = { x = ll.x; y = ur.y } in
	let endp = ur in
		{ startp; endp }
		
let rectangle_left_segment { ll; ur } = 
	let startp = { x = ll.x; y = ur.y } in
	let endp = ll in
		{ startp; endp }
		
let rectangle_right_segment { ll; ur } =
	let startp = ur in
	let endp = { x = ur.x; y = ll.y } in
		{ startp; endp }

type rectangle2 = { lx : float; ux : float; ly : float; uy : float }

let rectangle_lower_segment2 { lx; ux; ly; uy } = 
	let startp = { x = lx; y = ly } in
	let endp = { x = ux; y = ly } in
		{ startp; endp }
		
let rectangle_upper_segment2 { lx; ux; ly; uy } =
	let startp = { x = lx; y = uy } in
	let endp = { x = ux; y = uy } in
		{ startp; endp }
		
let rectangle_left_segment2 { lx; ux; ly; uy } =
	let startp = { x = lx; y = uy } in
	let endp = { x = lx; y = ly } in
		{ startp; endp }
		
let rectangle_right_segment2 { lx; ux; ly; uy } =
	let startp = { x = ux; y = uy } in
	let endp = { x = ux; y = ly } in
		{ startp; endp }
		
let rectangle_perimeter r =
	(segment_length (rectangle_lower_segment r))
		+. (segment_length (rectangle_upper_segment r))
		+. (segment_length (rectangle_left_segment r))
		+. (segment_length (rectangle_right_segment r))

let rectangle_perimeter2 r =
	(segment_length (rectangle_lower_segment2 r))
		+. (segment_length (rectangle_upper_segment2 r))
		+. (segment_length (rectangle_left_segment2 r))
		+. (segment_length (rectangle_right_segment2 r))

let rectangle_area r =
	(segment_length (rectangle_lower_segment r))
		*. (segment_length (rectangle_left_segment r))
		
let rectangle_area2 r =
	(segment_length (rectangle_lower_segment2 r))
		*. (segment_length (rectangle_left_segment2 r))

let make_rectangle ll ur = { ll; ur}

let make_rectangle2 lx ux ly uy = { lx; ux; ly; uy }


(* A.3 *)

let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*

first (make_pair x y) takes in (fun m -> m x y) as the argument for z, 
which takes (fun x y -> x) as the argument for m, which takes x and y
as arguments for x and y, yielding x as the result.

Evaluate second (make_pair 1 2):
  evaluate make_pair 1 2
	evaluate 1 -> 1
	evaluate 2 -> 2
	evaluate make_pair ->
	  fun x y -> fun m -> m x y
	apply fun x y -> ... to 1, 2
	  substitute 1 for x, 2 for y in body ->
	    fun m -> m 1 2
  evaluate second ->
    fun z -> z (fun x y -> y)
  apply fun z -> ... to (fun m -> m 1 2)
    substitute (fun m -> m 1 2) for z in body ->
      (fun m -> m 1 2) (fun x y -> y)
    evaluate:
      evaluate (fun x y -> y) -> (fun x y -> y)
      evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
      apply (fun m -> m 1 2) to (fun x y -> y)
        substitute (fun x y -> y) for m in body ->
          (fun x y -> y) 1 2
        evaluate:
          evaluate 1 -> 1
          evaluate 2 -> 2
          evaluate (fun x y -> y) -> (fun x y -> y)
          apply (fun x y -> y) to 1, 2
          substitute 1 for x, 2 for y in body ->
            2
Result: 2   

*)


(* A.4 *)

let rec pow a b =
	if b = 0
		then 1
		else a * (pow a (b - 1))

let int_log base n =
	let rec iter base n' log =
		if n' mod base <> 0
			then log
			else iter base (n' / base) (log + 1)
	in
		iter base n 0

let make_pairi a b = (pow 2 a) * (pow 3 b)
	
let firsti pair = int_log 2 pair

let secondi pair = int_log 3 pair


(* A.5 *)

let zero = []

let is_zero = function
	| [] -> true
	| () :: _ -> false
	
let succ u = () :: u

let prev = function
	| [] -> invalid_arg "Argument must be greater than zero"
	| _ :: t -> t

let rec integer_to_unary i =
	if i = 0
		then zero
		else succ (integer_to_unary (i - 1))
		
let rec unary_to_integer u =
	if is_zero u
		then 0
		else 1 + (unary_to_integer (prev u))

let unary_add u1 u2 =
	integer_to_unary ((unary_to_integer u1) + (unary_to_integer u2))

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
	| Zero -> true
	| Succ _ -> false
	
let succ' u = Succ u

let prev' = function
	| Zero -> invalid_arg "Argument must be greater than zero"
	| Succ u -> u
	
let rec integer_to_unary' i =
	if i = 0
		then zero'
		else succ' (integer_to_unary' (i - 1))
		
let rec unary_to_integer' u =
	if is_zero' u
		then 0
		else 1 + (unary_to_integer' (prev' u))

let unary_add' u1 u2 =
	integer_to_unary' ((unary_to_integer' u1) + (unary_to_integer' u2))

(* 
The other definitions don't have to change from their definitions in
the previous representation other than name changes.
*)


(* A.6 *)
let zerof = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)
let church_to_integer n = n (fun i -> 1 + i) 0


(* A.7 *)
(*

val zerof : 'a -> ('b -> 'b) = <fun>
val one : ('a -> 'b) -> ('a -> 'b) = <fun>
val church_to_integer : ((int -> int) -> (int -> 'c)) -> 'c = <fun>

When evaluating church_to_integer zerof, the type of 'a is a function
int -> int (corresponds to (fun i -> 1 + i), and the type of 'b is int. 
In the type of church_to_integer,
(int -> 'c) corresponds to ('b -> 'b) of zerof, so the type of 'c is
int as well.

When evaluating church_to_integer one, the type of 'a is int, which
corresponds to z = 0, and 'b corresponds to s z, and since s corresponds
to int -> int, the type of 'b is int. In the type of church_to_integer,
(int -> 'c) corresponds to ('a -> 'b) of one, so the type 'c is int
as well.

*)


(* B.1 *)
let rec last_sublist = function
	| [] -> invalid_arg "last_sublist: empty list"
	| [i] -> [i]
	| _ :: t -> last_sublist t


(* B.2 *)
let reverse lst =
	let rec iter orig rev =
		match orig with
			| [] -> rev
			| h :: t -> iter t (h :: rev) 
	in
		iter lst []


(* B.3 *)
let rec square_list = function
	| [] -> []
	| h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun i -> i * i) items


(* B.4 *)
(*

The answer list is in the reverse order because he processes the list
from the beginning, squaring each item, and consing it to the beginning
of the answer list, resulting in the last element of the list at the
beginning of the answer list.

Intechanging the arguments to the :: constructor doesn't work because
the left operand of :: has to be an element of the list, and the right
operand has to be a list. (h * h) is not of type list.

We can change (answer :: (h * h)) to (answer @ [h * h]), but the
resulting function would not be efficient because the @ operator is
not tail-recursive.

*)


(* B.5 *)
let rec count_negative_numbers = function
	| [] -> 0
	| h :: t -> 
		if h < 0 
			then 1 + count_negative_numbers t
			else count_negative_numbers t

let power_of_two_list n =
	let rec iter i =
		if i = n
			then []
			else (pow 2 i) :: (iter (i + 1))
	in
		iter 0
		
let prefix_sum lst =
	let rec iter add cur =
		match cur with
			| [] -> []
			| h :: t -> 
				let h' = h + add in
					h' :: iter h' t
	in
		iter 0 lst
		
		
(* B.6 *)
let deep_reverse lst =
	let rec shallow_reverse orig' rev' =
		match orig' with
			| [] -> rev'
			| h :: t -> shallow_reverse t (h :: rev')
	in
	let rec iter orig rev =
		match orig with
			| [] -> rev
			| h :: t -> iter t ((shallow_reverse h []) :: rev) 
	in
		iter lst []


(* B.7 *)

type 'a nested_list = Value of 'a | List of 'a nested_list list

let rec deep_reverse_nested lst =
	let rec iter orig rev =
		match orig with
			| [] -> rev
			| h :: t -> iter t ((deep_reverse_nested h) :: rev)
	in
	let reverse_list l' =
		iter l' []
	in
		match lst with
			| Value i -> Value i
			| List l -> List (reverse_list l) 
