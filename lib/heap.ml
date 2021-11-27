type index_t = int ref

(* int ref: size of data
   index_t: index
   'a:      key
   'b:      value *)
type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

let create size key value = (ref 0, Array.make size (ref (-1), key, value))
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
  let%test "can swap" = arr = [|(ref 0, 2.2, "23"); (ref 1, 1.1, "12"); (ref 2, 3.3, "34"); (ref 3, 4.4, "45");|]
  let _ = swap arr 1 3
  let%test "can swap" = arr = [|(ref 0, 2.2, "23"); (ref 1, 4.4, "45"); (ref 2, 3.3, "34"); (ref 3, 1.1, "12");|]
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
  let%test "can adjust" = arr
    = [|        (ref 0, 1.1, "12");
      (ref 1, 2.2, "23"); (ref 2, 3.3, "34");
    (ref 3, 4.4, "45");|]
end)

let insert (size_ref, data) key value =
  let index_ref = ref !size_ref in
  data.(!size_ref) <- (index_ref, key, value);
  size_ref := !size_ref + 1;
  adjust_parent data !index_ref;
  (index_ref, (size_ref, data)) (* TODO *)

let%test_module "insert" = (module struct
  let heap = create 7 infinity ""
  let (size, data) = heap

  let%test "can create initialized heap" = data = [|
                                          (ref (-1), infinity, "");
                (ref (-1), infinity, "");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size = 0

  let (index, heap) = insert heap 4.2 "hi"
  let (size, data) = heap
  let%test "can insert an element into empty heap" = data = [|
                                            (ref 0, 4.2, "hi");
                (ref (-1), infinity, "");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size = 1
  let%test _ = !index = 0

  let (index, heap) = insert heap 4.7 "greater"
  let (size, data) = heap
  let%test "can insert an element into not empty heap" = data = [|
                                             (ref 0, 4.2, "hi");
                 (ref 1, 4.7, "greater");                          (ref (-1), infinity, "");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size = 2
  let%test _ = !index = 1

  let (index, heap) = insert heap 2.0 "small"
  let (size, data) = heap
  let%test "can insert an element into not empty heap" = data = [|
                                             (ref 0, 2.0, "small");
                  (ref 1, 4.7, "greater");                              (ref 2, 4.2, "hi");
    (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size = 3
  let%test _ = !index = 0

  let (index, heap) = insert heap 3.0 "medium"
  let (size, data) = heap
  let%test "can insert an element into not empty heap" = data = [|
                                             (ref 0, 2.0, "small");
                  (ref 1, 3.0, "medium");                             (ref 2, 4.2, "hi");
    (ref 3, 4.7, "greater"); (ref (-1), infinity, ""); (ref (-1), infinity, ""); (ref (-1), infinity, "");
  |]
  let%test _ = !size = 4
  let%test _ = !index = 1
end)
