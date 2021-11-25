type ('a, 'b) t = Empty
                | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

let empty = Empty

let rec insert tree k v = match tree with
    Empty -> Node (Empty, k, v, Empty)
  | Node (left, key, value, right) ->
      if k = key then Node (left, key, v, right)
      else if k < key
           then Node (insert left k v, key, value, right)
           else Node (left, key, value, insert right k v)

let rec search tree k = match tree with
    Empty -> raise Not_found
  | Node (left, key, value, right) ->
      if k = key then value
      else if k < key then search left k
                      else search right k
