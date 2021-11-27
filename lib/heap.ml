type index_t = int ref

(* int ref: size of data
   index_t: index
   'a:      key
   'b:      value *)
type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

let create max key value = (ref 0, Array.make max (ref (-1), key, value))
(* initial index is `-1` *)

(* swap : (index_t * 'a * 'b) array -> int -> int -> unit *)
let swap arr current_index parent_index =
  let (index_ref_c, _, _) as c = arr.(current_index) in
  let (index_ref_p, _, _) as p = arr.(parent_index) in
  index_ref_c := parent_index;
  index_ref_p := current_index;
  arr.(current_index) <- p;
  arr.(parent_index) <- c;
  ()

let%test_module "swap" = (module struct
  let arr = [|(ref 0, 1.1, "12"); (ref 1, 2.2, "23"); (ref 2, 3.3, "34"); (ref 3, 4.4, "45");|]

  let _ = swap arr 0 1
  let%test _ = arr = [|(ref 0, 2.2, "23"); (ref 1, 1.1, "12"); (ref 2, 3.3, "34"); (ref 3, 4.4, "45");|]
  let _ = swap arr 1 3
  let%test _ = arr = [|(ref 0, 2.2, "23"); (ref 1, 4.4, "45"); (ref 2, 3.3, "34"); (ref 3, 1.1, "12");|]
end)

(* Destructively adjust array. *)
(* adjust_children : int -> (index_t * 'a * 'b) array -> int -> unit *)
let rec adjust_children size arr current_index =
  if current_index >= size
  then ()
  else let current = arr.(current_index) in
       let (_, key_c, _) = current in
       (* left child *)
       let c_left_index = current_index * 2 + 1 in
       if c_left_index >= size
       then ()
       else let (_, key_left, _) = arr.(c_left_index) in
       if key_c > key_left
       then (swap arr c_left_index current_index;
             adjust_children size arr c_left_index;
             ())
       else
       (* right child *)
       let c_right_index = current_index * 2 + 2 in
       if c_right_index >= size
       then ()
       else
       let (_, key_right, _) = arr.(c_right_index) in
       if key_c > key_right
       then (swap arr c_right_index current_index;
             adjust_children size arr c_right_index;
             ())
       else ()

let%test_module "adjust_children" = (module struct
  let arr = [|
                                        (ref 0, 2.2, "23");
               (ref 1, 4.4, "45");                              (ref 2, 3.3, "34");
    (ref 3, 1.1, "12");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]
  let _ = adjust_children 4 arr 1
  let%test "left one" = arr
    = [|
                                        (ref 0, 2.2, "23");
               (ref 1, 1.1, "12");                              (ref 2, 3.3, "34");
    (ref 3, 4.4, "45");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]

  let arr = [|
                                        (ref 0, 9.9, "23");
               (ref 1, 2.2, "45");                              (ref 2, 3.3, "34");
    (ref 3, 4.4, "12");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]
  let _ = adjust_children 4 arr 0
  let%test "left multi" = arr
    = [|
                                        (ref 0, 2.2, "45");
               (ref 1, 4.4, "12");                              (ref 2, 3.3, "34");
    (ref 3, 9.9, "23");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]

  let arr = [|
                                        (ref 0, 4.0, "23");
               (ref 1, 4.4, "45");                              (ref 2, 3.3, "34");
    (ref 3, 1.1, "12");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]
  let _ = adjust_children 4 arr 0
  let%test "right" = arr
    = [|
                                        (ref 0, 3.3, "34");
               (ref 1, 4.4, "45");                              (ref 2, 4.0, "23");
    (ref 3, 1.1, "12");(ref (-1), infinity, "");(ref (-1), infinity, "");(ref (-1), infinity, "");
  |]
end)

(* Destructively adjust array. *)
(* adjust_parent : (index_t * 'a * 'b) array -> int -> unit *)
let rec adjust_parent arr current_index =
  if current_index = 0
  then ()
  else let current = arr.(current_index) in
       let (_, key_c, _) = current in
       let parent_index = (current_index - 1) / 2 in
       let parent = arr.(parent_index) in
       let (_, key_p, _) = parent in
       if key_p < key_c
       then ()
       else swap arr current_index parent_index;
            adjust_parent arr parent_index;
            ()

let%test_module "adjust_parent" = (module struct
  let arr = [|  (ref 0, 2.2, "23");
       (ref 1, 4.4, "45"); (ref 2, 3.3, "34");
    (ref 3, 1.1, "12");|]

  let _ = adjust_parent arr 3
  let%test _ = arr
    = [|        (ref 0, 1.1, "12");
      (ref 1, 2.2, "23"); (ref 2, 3.3, "34");
    (ref 3, 4.4, "45");|]
end)

let insert (size_ref, arr) key value =
  let index_ref = ref !size_ref in
  arr.(!size_ref) <- (index_ref, key, value);
  size_ref := !size_ref + 1;
  adjust_parent arr !index_ref;
  (index_ref, (size_ref, arr))

let%test_module "create & insert" = (module struct
  let heap = create 7 infinity ""
  let (size_ref, arr) = heap

  let%test "can create initialized heap" = arr = [|
                                          (ref (-1), infinity, "");
                (ref (-1), infinity, "");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size_ref = 0

  let (index, heap) = insert heap 4.2 "hi"
  let (size_ref, arr) = heap
  let%test "can insert an element into empty heap" = arr = [|
                                            (ref 0, 4.2, "hi");
                (ref (-1), infinity, "");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size_ref = 1
  let%test _ = !index = 0

  let (index, heap) = insert heap 4.7 "greater"
  let (size_ref, arr) = heap
  let%test "can insert an element into not empty heap" = arr = [|
                                             (ref 0, 4.2, "hi");
                 (ref 1, 4.7, "greater");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size_ref = 2
  let%test _ = !index = 1

  let (index, heap) = insert heap 2.0 "small"
  let (size_ref, arr) = heap
  let%test "can insert an element into not empty heap" = arr = [|
                                             (ref 0, 2.0, "small");
                  (ref 1, 4.7, "greater");                              (ref 2, 4.2, "hi");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size_ref = 3
  let%test _ = !index = 0

  let (index, heap) = insert heap 3.0 "medium"
  let (size_ref, arr) = heap
  let%test "can insert an element into not empty heap" = arr = [|
                                             (ref 0, 2.0, "small");
                  (ref 1, 3.0, "medium");                             (ref 2, 4.2, "hi");
    (ref 3, 4.7, "greater"); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size_ref = 4
  let%test _ = !index = 1
end)

let get (_, arr) index_ref =
  let (_, k, v) = arr.(!index_ref) in
  (k, v)

let set (size_ref, arr) index_ref key value =
  let (_, k_pre, _) = arr.(!index_ref) in
  (arr.(!index_ref) <- (index_ref, key, value);
   if key < k_pre
   then adjust_parent arr !index_ref
   else adjust_children !size_ref arr !index_ref;
   (size_ref, arr))

let%test_module "get & set" = (module struct
  let heap =
    (ref 4, [|
                         (ref 0, 1.1, "12");
                (ref 1, 2.2, "23"); (ref 2, 3.3, "34");
      (ref 3, 4.4, "45");|])

  let%test _ = get heap (ref 0) = (1.1, "12")
  let%test _ = get heap (ref 2) = (3.3, "34")

  let%test _ = set heap (ref 2) 7.7 "77"
    = (ref 4, [|
                       (ref 0, 1.1, "12");
             (ref 1, 2.2, "23"); (ref 2, 7.7, "77");
    (ref 3, 4.4, "45");|])
  let%test "write destructively" = heap
    = (ref 4, [|
                       (ref 0, 1.1, "12");
             (ref 1, 2.2, "23"); (ref 2, 7.7, "77");
    (ref 3, 4.4, "45");|])
  let%test _ = set heap (ref 1) 0.1 "00"
    = (ref 4, [|
                         (ref 0, 0.1, "00");
               (ref 1, 1.1, "12"); (ref 2, 7.7, "77");
      (ref 3, 4.4, "45");|])
  let%test "write destructively" = heap
    = (ref 4, [|
                         (ref 0, 0.1, "00");
               (ref 1, 1.1, "12"); (ref 2, 7.7, "77");
      (ref 3, 4.4, "45");|])
  let%test _ = set heap (ref 0) 9.9 "99"
    = (ref 4, [|
                         (ref 0, 1.1, "12");
               (ref 1, 4.4, "45"); (ref 2, 7.7, "77");
      (ref 3, 9.9, "99");|])
  let%test "write destructively" = heap
    = (ref 4, [|
                         (ref 0, 1.1, "12");
               (ref 1, 4.4, "45"); (ref 2, 7.7, "77");
      (ref 3, 9.9, "99");|])
end)
