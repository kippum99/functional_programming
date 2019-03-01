(* A.1 *)

let fibonacci n =
	let i = ref 2 in
	let first = ref 0 in
	let second = ref 1 in
	let third = ref 1 in
		if n < 2 then n
		else
			begin
				while !i <= n do
					third := !first + !second;
					first := !second;
					second := !third;
					i := !i + 1
				done;
				!third
			end

let fibonacci2 n =
	let first = ref 0 in
	let second = ref 1 in
	let third = ref 1 in
		if n < 2 then n
		else
			begin
				for i = 2 to n do
					third := !first + !second;
					first := !second;
					second := !third;
				done;
				!third
			end	


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
		| (`Length a', `Length b') -> length_add a' b'
		| (`Mass a', `Mass b') -> mass_add a' b'
		| (`Time a', `Time b') -> time_add a' b'
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

