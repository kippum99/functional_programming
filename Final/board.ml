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
        
    (* Get all possible locs a knight's move away on board from the
     * given loc (r, c) *)
    let get_next_locs board (r, c) =
      let rec iter moves ans =
        match moves with
          | [] -> ans
          | (r', c') :: t ->
            let loc = (r + r', c + c') in
              if check_bounds board loc
                then iter t (loc :: ans)
                else iter t ans (* Discard if off board *)
      in
        iter possible_moves []
	
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
          (* Iterate through all locs a Knight's move away on board and 
           * add (loc, reachability) if loc is unoccupied *)
          let rec iter locs ans =
            match locs with
              | [] -> ans
              | h :: t ->
                match get_reachable board h with
                    | None -> iter t ans (* Discard if null loc *)
                    | Some i -> iter t ((h, i) :: ans)
          in
            iter (get_next_locs board loc) [] 
    
    let place board loc =
      if not(check_bounds board loc) then
        bad_loc "place" "location off board" loc
      else if get_index board loc <> None then
        bad_loc "place" "location is already occupied" loc
      else
        (* Helper function that returns true if it a knight's move away
         * form the last placed knight, false otherwise *)
        let is_knights_move (r, c) = 
          let (r', c') = get_last board in
          let (r_dist, c_dist) = (abs (r - r'), abs (c - c')) in
            (r_dist = 2 && c_dist = 1) || (r_dist = 1 && c_dist = 2)
        in
          if board.last_index <> 0 && not (is_knights_move loc)
            then bad_loc "place" "loc is not a knight's move" loc
      else (* If the placement is valid *)
        (* Update placed list *)
        let placed = loc :: board.placed in
        (* Update last index *)
        let last_index = board.last_index + 1 in
        (* Update indices grid *)
        let indices = S.set board.indices loc last_index in
        (* Update reachable grid *)
        let rec iter locs grid = 
          match locs with
            | [] -> grid
            | h :: t ->
              (* Decrement reachability by 1 if not null *)
              match get_reachable board h with
                | None -> iter t grid
                | Some i -> iter t (S.set grid h (i - 1)) 
        in
        (* Set loc to null in reachable and update all non-null locs
         * a Knight's move away in reachable *)
        let reachable = 
          iter (get_next_locs board loc) (S.remove board.reachable loc)
        in
          { board with placed; last_index; indices; reachable }
        

    let undo board = 
      (* Return the input board if there was no last move *)
      if board.last_index = 0 then
        board
      else
        let last_loc = List.hd board.placed in
        (* Update placed list *)
        let placed = List.tl board.placed in
        (* Update last index *)
        let last_index = board.last_index - 1 in
        (* Update indices grid *)
        let indices = S.remove board.indices last_loc in
        (* Update reachable grid *)
        let rec iter locs grid = 
          match locs with
            | [] -> grid
            | h :: t ->
              (* Increment reachability by 1 if not null *)
              match get_reachable board h with
                | None -> iter t grid
                | Some i -> iter t (S.set grid h (i + 1)) 
        in
        (* Helper function to recompute reachability of loc *)
        let rec count_reachability moves count =
          match moves with
            | [] -> count
            | h :: t -> 
              let cur = if get_index board h = None then 1 else 0 in
                count_reachability t (count + cur)
        in
        (* Update reachability of loc and update all non-null locs a
         * Knight's move away in reachable *)
        let next_locs = get_next_locs board last_loc in
        let count = count_reachability next_locs 0 in
        let reachable' = S.set board.reachable last_loc count in
        let reachable = iter next_locs reachable' in
          { board with placed; last_index; indices; reachable }
          
    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
