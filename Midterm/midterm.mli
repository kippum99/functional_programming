(* midterm.mli: interface file for the CS 4 midterm exam, 2019 *)

(* 2.1 *)

val split3 : int list -> int list * int list * int list
val merge3 : int list -> int list -> int list -> int list

(* 2.2 *)

val smallest_index : int list -> int
val flip_n : int -> int list -> int list
val block_sort1 : int list -> int list
val block_sorti : int list -> int list

(* 3.1 *)

val linrec :
  ('a -> bool) ->
  ('a -> 'b) -> ('a -> 'c * 'a) -> ('c -> 'b -> 'b) -> 'a -> 'b

val insert_r : int -> int list -> int list
val insertion_sort : int list -> int list

(* 3.2 *)

val binrec :
  ('a -> bool) ->
  ('a -> 'b) -> ('a -> 'c * 'a * 'a) -> ('c -> 'b -> 'b -> 'b) -> 'a -> 'b

val quicksort : int list -> int list

(* 3.3 *)

val tailrec : ('a -> bool) -> ('a -> 'b) -> ('a -> 'a) -> 'a -> 'b
val insert_i : int -> int list -> int list
val insertion_sort_i : int list -> int list

(* Section 4. *)

(** AA trees of integers.
    The Node contents are:
    -- level
    -- value (an integer)
    -- left subtree
    -- right subtree
 *)
type tree = 
  | Leaf 
  | Node of int * int * tree * tree

val member : int -> tree -> bool
val skew   : tree -> tree
val split  : tree -> tree
val insert : int -> tree -> tree

