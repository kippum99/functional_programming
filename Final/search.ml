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

    (* Helper functions go here. *)

    let search nrows ncols start_row start_col print =
      failwith "TODO"
  end

