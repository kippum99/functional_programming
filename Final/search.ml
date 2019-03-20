(* Student name: Joo Eun (June) Kim *)
(* CMS cluster login name: kooeun *)


open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions. *)
    
    (* Return a list of locs that have the minimum reachability count in 
     * (loc, count) list *)
    let rec get_min_count_locs loc_counts min locs =
      match loc_counts with
        | [] -> locs
        | (loc, count) :: t ->
          match () with
            | _ when count > min -> get_min_count_locs t min locs
            | _ when count = min -> 
              get_min_count_locs t min (loc :: locs)
            | _ -> get_min_count_locs t count [loc] (* count < min *)
              
    (* Interface functions. *)

    let search nrows ncols start_row start_col print =
      let rec iter board =
        if B.is_solved board then
          (* Print board if print is true and return placed locs *)
          begin
            if print then P.print_board board false; 
            Some (B.get_placed board)
          end
        else
          let last_loc = B.get_last board in
          (* Get all locs that can be reached from last_loc with their
           * reachability counts *)
          let loc_counts = B.get_loc_counts_from_loc board last_loc in
            if loc_counts = [] then None (* Search fails *)
            else
              (* Place a knight on a random loc from loc_counts with min 
               * reachability count *)
              let locs = get_min_count_locs loc_counts 8 [] in
              let rand_idx = Random.int (List.length locs) in
              let rand_loc = List.nth locs rand_idx in
                iter (B.place board rand_loc)
      in     
        iter (B.place (B.make nrows ncols) (start_row, start_col))
  end

