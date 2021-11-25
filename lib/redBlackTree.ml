(* color type *)
type color_t = Red | Black

(* Red-black tree type
   that has key ('a), value ('b), and color type. *)
type ('a, 'b) t = Empty
               | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

let empty = Empty

(* Blance tree. *)
(* blance : ('a, 'b) t -> ('a, 'b) t *)
let blance tree = match tree with
  | Node (Node (Node (t0, k0, v0, Red, t1), k1, v1, Red, t2), k2, v2, Black, t3)
  | Node (Node (t0, k0, v0, Red, Node (t1, k1, v1, Red, t2)), k2, v2, Black, t3)
  | Node (t0, k0, v0, Black, Node (Node (t1, k1, v1, Red, t2), k2, v2, Red, t3))
  | Node (t0, k0, v0, Black, Node (t1, k1, v1, Red, Node (t2, k2, v2, Red, t3)))
      -> Node(Node(t0, k0, v0, Black, t1), k1, v1, Red, Node(t2, k2, v2, Black, t3))
  | _ -> tree

let insert tree key value =
  let rec ins t0 k0 v0 = match t0 with
      Empty -> Node (Empty, k0, v0, Red, Empty)
    | Node (left, k, v, c, right) ->
        let t0 =
          if k0 < k then Node ((ins left k0 v0), k, v, c, right)
          else if k < k0 then Node (left, k, v, c, (ins right k0 v0))
          else Node (left, k, v0, c, right) in
        blance t0 in
  match ins tree key value with
      Empty -> assert false
    | Node (left, k, v, _, right) -> Node (left, k, v, Black, right)

let rec search tree k = match tree with
    Empty -> raise Not_found
  | Node (left, key, value, _, right) ->
      if k = key then value
      else if k < key then search left k
                      else search right k
