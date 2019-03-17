(* search.ml: search strategies *)
(* Student name: Joo Eun (June) Kim *)
(* CMS cluster login name: kooeun *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = 
      (* Create a new Storage structure to store board histories *)
      let storage = S.create () in
      (* Keep searching until storage is empty or solution is found *)
      let rec iter visited =
        if S.is_empty storage
          then raise Not_found
          else 
            let next_history = S.pop storage in
            let recent_board = List.hd next_history in
              if DS.mem recent_board visited (* if already seen *)
                then iter visited
                else
                  (* If the board solves the problem, return history *)
                  if D.is_solved recent_board
                    then next_history
                    else
                      (* Helper function that generates a history and
                       * pushes onto storage *)
                      let push_history b =
                        let history = b :: next_history in
                          S.push history storage
                      in
                        (* Update visited, storage, and keep searching 
                         *)
                        List.iter push_history (D.next recent_board);
                        iter (DS.add recent_board visited)  
      in
        (* Create a history with just the initial board and push onto
         * storage *)
        S.push [init] storage;
        iter DS.empty (* set of visited boards is initially empty *)

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

