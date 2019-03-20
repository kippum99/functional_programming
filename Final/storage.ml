(* Student name: Joo Eun (June) Kim *)
(* CMS cluster login name: kooeun *)


module Loc =
  struct
    type t = int * int

    let compare = Pervasives.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
      if nrows <= 0 || ncols <= 0
        then 
          invalid_arg (Printf. sprintf 
            "make: invalid arguments: nrows = %d, ncols = %d"
            nrows ncols)
        else Array.make_matrix nrows ncols (-1)

    let get data (row, col) =
      try 
        let i = data.(row).(col) in
          if i = -1 (* Location is null loc *)
            then None
            else Some i
      with
        (* If location is off the grid *)
        Invalid_argument _ -> None
        
    let set data (row, col) i = 
      if i < 0
        then invalid_arg "set: negative argument"
        else
          try 
            data.(row).(col) <- i;
            data
          with
            (* If location is off the grid *)
            Invalid_argument _ -> 
              invalid_arg (Printf.sprintf 
                "set: invalid loation: (%d, %d)" row col)

    let has_loc data (row, col) =
      try
        if data.(row).(col) <> -1
          then true
          else false (* If location is null loc *)
      with
        Invalid_argument _ -> false (* If location is off the grid *)

    let remove data (row, col) =
      try
        data.(row).(col) <- -1;
        data
      with
        (* Return input storage unchanged if location is off the grid *)
        Invalid_argument _ -> data
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = 
      if nrows <= 0 || ncols <= 0
        then 
          invalid_arg (Printf. sprintf 
            "make: invalid arguments: nrows = %d, ncols = %d"
            nrows ncols)
        else { contents = LocMap.empty; nrows; ncols }
    
    let get data (row, col) = 
      (* If location is off the grid *)
      if row < 0 || row >= data.nrows || col < 0 || col >= data.ncols
        then None
        else 
          try Some (LocMap.find (row, col) data.contents)
          with Not_found -> None (* Location is null loc *)

    let set data (row, col) i = 
      if row < 0 || row >= data.nrows || col < 0 || col >= data.ncols
        then invalid_arg 
              (Printf.sprintf "set: invalid loation: (%d, %d)" row col)
        else if i < 0 
          then invalid_arg "set: negative argument"
          else
            (* Store i into the storage at given loc *)
            let contents =
              LocMap.update (row, col) (fun _ -> Some i) data.contents
            in
              { data with contents }

    let has_loc data (row, col) = 
      LocMap.mem (row, col) data.contents

    let remove data (row, col) =
      let contents = LocMap.remove (row, col) data.contents in
        { data with contents }
  end

