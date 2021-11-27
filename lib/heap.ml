(* int ref: size of data
   'a:      key
   'b:      value *)
type ('a, 'b) t = int ref * ('a * 'b) array

type index_t = int ref

let create size key value = (ref 0, Array.make size (key, value))

let%test_module _ = (module struct
  let heap = create 15 0 ""
  let (size, data) = heap

  let%test "can create initialized heap" = data = [|
                                  (0, "");
                 (0, "");                          (0, "");
        (0, "");          (0, "");        (0, "");          (0, "");
    (0, ""); (0, ""); (0, "");(0, "");(0, ""); (0, ""); (0, "");(0, "");
  |]
  let%test _ = !size = 0

  (*
  let new_heap = insert heap 10 "hi"
  let%test "returns inserted heap" = new_heap = [|
                                   (0, "");
                 (0, "");                            (0, "");
        (0, "");          (0, "");          (0, "");          (0, "");
    (0, ""); (0, ""); (0, "");(10, "hi");
  |]
  let%test "destructively change heap" = heap = [|
                                   (0, "");
                 (0, "");                            (0, "");
        (0, "");          (0, "");          (0, "");          (0, "");
    (0, ""); (0, ""); (0, "");(10, "hi");
  |]
  *)
end)
