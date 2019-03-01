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
			for i = 0 to (Array.length arr) - 2 do
				if arr.(i) > arr.(i + 1) then
					let temp = arr.(i) in
						arr.(i) <- arr.(i + 1);
						arr.(i + 1) <- temp;
						swapped := true
			done
		done
					
					
(* B.1.a *)
let meters_per_foot = 0.3048

let get_meters len =
	match len with
	  | `Meter m -> m
	  | `Foot f -> f *. meters_per_foot
	  | `Inch i -> i /. 12. *. meters_per_foot
  
let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.1.b *)

