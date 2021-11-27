(* Min heap *)

type ('a, 'b) t
  (* 'a: key
     'b: value *)

type index_t
  (* index of heap *)

val create : int -> 'a -> 'b -> ('a, 'b) t
  (* usage: create size key value *)
  (* Takes size, key, and value, returns empty heap. *)

val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
  (* usage: insert heap key value *)
  (* Insert a new element into heap.
     (Write the heap destructively.) *)

(*
val get : ('a, 'b) t -> index_t -> 'a * 'b
  (* usage: get heap index *)
  (* Get an element of the index. *)

val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
  (* usage: set heap index key value *)
  (* Set key and value to the index of the heap.
     (Write the heap destructively.) *)

val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (* usage: split_top heap *)
  (* Returns an element of the minimum key and heap.
     (Write the heap destructively.) *)
*)
