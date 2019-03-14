(* klotski.ml: core functionality of the Klotski game. *)
(* Student name: Joo Eun (June) Kim *)
(* CMS cluster login name: kooeun *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Pervasives.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b =
  (* Target location for 2x2 piece in bottom 2 rows, 2 center cols *)
  let target = LocSet.of_list [(3, 1); (3, 2); (4, 1); (4, 2)] in
    CharMap.exists (fun c locs -> LocSet.equal locs target) b.pieces

let compare b1 b2 =
  (* Compare sets of unoccupied locs *)
  let comp_u = LocSet.compare b1.unoccupied b2.unoccupied in
    if comp_u <> 0 then
			comp_u
	(* Compare sets of pieces if unoccupied locs are the same *)
		else
			(* Helper function to extract set of LocSets from CharMap.t *)
			let rec extract_pieces = function
				| [] -> LocSetSet.empty
				| (_, locs) :: t -> LocSetSet.add locs (extract_pieces t)
			in
				let p1 = extract_pieces (CharMap.bindings b1.pieces) in
				let p2 = extract_pieces (CharMap.bindings b2.pieces) in
					LocSetSet.compare p1 p2

let remove c ({ pieces = p; unoccupied = u } as b) = 
  failwith "TODO"

let add (c, p) { pieces = ps; unoccupied = u } = 
  failwith "TODO"

let make_move (c, d, i) b =
  failwith "TODO"

let next b =
  failwith "TODO"

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

