(* Student name: Joo Eun (June) Kim *)
(* CMS cluster login name: kooeun *)


open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc
      
    (* List of possible knight moves *)
	  let possible_moves = 
	    [(-2, -1); (-1, -2); (1, -2); (2, -1); (2, 1); (1, 2); (-1, 2);
	      (-2, 1)] 
	
    let init_reachable nrows ncols =
      (* Helper function to count reachability from (r, c) *)
      let rec count_reachability r c count moves =
        match moves with
          | [] -> count
          | (r', c') :: t -> 
            let cur = 
              if ok_loc nrows ncols (r + r', c + c') then 1 else 0
            in
              count_reachability r c (count + cur) t
      in
      (* Iterate through the Storage grid and fill it up with
       * reachability counts *)
      let rec iter r c grid =
        if r = nrows then grid
        else if c = ncols
          then iter (r + 1) 0 grid
          else
            let reachability = count_reachability r c 0 possible_moves
            in
            let new_grid = S.set grid (r, c) reachability in
              iter r (c + 1) new_grid
      in
        iter 0 0 (S.make nrows ncols)

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    let get_loc_counts_from_loc board loc = 
      if not(check_bounds board loc)
        then bad_loc "get_loc_counts_from_loc" "location off board" loc
        else
          let rec iter locs ans =
            match locs with
              | [] -> ans
              | h :: t ->
                try
                  match get_reachable board h with
                    | None -> iter t ans (* Discard if null loc *)
                    | Some i -> iter t ((h, i) :: ans)
                with
                  (* Discard if loc off board *)
                  Invalid_argument _ -> iter t ans
          in
          let (r, c) = loc in
          let next_locs = 
            List.map (fun (r', c') -> (r + r', c + c')) possible_moves
          in
            iter next_locs [] 
    
    let place board loc = 
      failwith "TODO"

    let undo board = 
      failwith "TODO"

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
