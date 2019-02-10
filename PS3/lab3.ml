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

let print_post { x; y} =
	Printf.printf "(%g, %g)" x y


(* A.2 *)

type rectangle = 
