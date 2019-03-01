(* A.1 *)

let fibonacci n =
	let i = ref 2 in
	let first = ref 0 in
	let second = ref 1 in
		if n < 2 then n
		else
			begin
				while !i <= n do
					let cur = !first + !second in
						first := !second;
						second := cur;
						i := !i + 1
				done;
				!second
			end

let fibonacci2 n =
	if n < 2 then n
	else
		let first = ref 0 in
		let second = ref 1 in
			for i = 2 to n do
				let cur = !first + !second in
					first := !second;
					second := cur
			done;
			!second


(* A.2 *)
let bubble_sort arr =
	let swapped = ref true in
		while !swapped do
			swapped := false;
			for i = 0 to Array.length arr - 2 do
				if arr.(i) > arr.(i + 1) then
					let temp = arr.(i) in
						arr.(i) <- arr.(i + 1);
						arr.(i + 1) <- temp;
						swapped := true
			done
		done
					
					
(* B.a *)
let meters_per_foot = 0.3048

let get_meters len =
	match len with
	  | `Meter m -> m
	  | `Foot f -> f *. meters_per_foot
	  | `Inch i -> i /. 12. *. meters_per_foot
  
let length_add a b = `Meter (get_meters a +. get_meters b)


(* B.b *)
let grams_per_slug = 14593.903203

let get_grams mass = 
	match mass with
		| `Gram g -> g
		| `Kilo k -> k *. 1000.
		| `Slug s -> s *. grams_per_slug
		
let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time =
	match time with
		| `Second s -> s
		| `Minute m -> m *. 60.
		| `Hour h -> h *. 3600.
		| `Day d -> d *. 24. *. 3600.
		
let time_add a b = `Second (get_seconds a +. get_seconds b)


(* B.c *)
let unit_add a b =
	match (a, b) with
		| (`Length a', `Length b') -> `Length (length_add a' b')
		| (`Mass a', `Mass b') -> `Mass (mass_add a' b')
		| (`Time a', `Time b') -> `Time (time_add a' b')
		| _ -> failwith "Data values are not compatible"
		
(*
We don't get into a combinatorial explosion when adding more unit
classes, because adding a unit class simply requires adding one line
of code to the unit_add function. We don't have to consider all the
combinations of different unit classes because the wild card _ case in
the match expression takes care of all the incompatible pairs of
unit types.
*)


(* C.1 *)
let rec make_gram g =
	let compatible_method other =
		match other#unit_type with
			| `Gram -> true
			| `Slug -> true
			| _ -> false
	in
	let add_method other =
		if compatible_method other then
			make_gram (g +. other#get_grams)
		else failwith "Incompatible data types"
	in
		object
			method get_grams = g
			method get_slugs = g /. grams_per_slug
			method unit_type = `Gram
			method compatible other = compatible_method other
			method add other = add_method other
		end


(* C.2 *)

(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
	object
		method value = i
		method show = string_of_int i
		method is_zero = i = 0
		method is_number = true
		(* must evaluate to an object *)
		method evaluate _ _ = make_number i 
		(* derivative of a number is 0 *)
		method derive _ = make_number 0 
	end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
	object
		method value = failwith "variable has no numerical value"
		method show  = v
		method is_zero = false
		method is_number = false
		method evaluate v' n =
			if v = v'
			then make_number n
			else make_variable v
		method derive v' =
			if v = v'
			then make_number 1  (* d/dx(x) = 1 *)
			else make_number 0  (* d/dx(y) = 0 *)
	end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
	match () with
		| _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
		| _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
		| _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
			make_number (expr1#value + expr2#value)
		| _ ->  (* create a new object representing the sum *)
			object
				method value = 
					failwith "sum expression has no numerical value"
				method show = 
					"(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
				method is_zero = false
				method is_number = false
				method evaluate v n = 
					make_sum (expr1#evaluate v n) (expr2#evaluate v n)
				method derive v = 
					make_sum (expr1#derive v) (expr2#derive v)
			end

(* Evaluate a message-passing expression with a number 
 substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* C.2.a *)
let rec make_product expr1 expr2 =
	match () with
		| _ when expr1#is_zero -> make_number 0  (* 0 * expr = 0 *)
		| _ when expr2#is_zero -> make_number 0  (* expr * 0 = 0 *)
		| _ when expr1#is_number && expr2#is_number ->
			make_number (expr1#value * expr2#value)
		| _ when expr1#is_number && expr1#value = 1 -> expr2
		| _ when expr2#is_number && expr2#value = 1 -> expr1
		| _ ->  (* create a new object representing the product *)
			object
				method value = failwith 
					"product expression has no numerical value"
				method show = 
					"(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
				method is_zero = false
				method is_number = false
				method evaluate v n = 
					make_product 
						(expr1#evaluate v n) (expr2#evaluate v n)
				method derive v =
					make_sum
						(make_product (expr1#derive v) (expr2))
						(make_product (expr1) (expr2#derive v))
			end

(* C.2.b *)
(*

1. 
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
  
2.
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
  
3.
- : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + 
(x * (y * y)))))"

4.
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"

5.
- : string = "558"

6. 
- : string = "396"

*)

