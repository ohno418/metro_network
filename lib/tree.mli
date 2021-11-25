(* Tree module *)

type ('a, 'b) t
  (* Tree type which has key ('a) and value (b). *)

val empty : ('a, 'b) t
  (* An empty tree. *)

val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* usage: insert tree key value *)
  (* Insert key and value into tree, and return it.
     If key already exists, overwrite the value. *)

val search : ('a, 'b) t -> 'a -> 'b
  (* usage: search tree key *)
  (* Search value for the key in the tree.
     Raise Not_found if not found the key. *)
