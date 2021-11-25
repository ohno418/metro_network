type ekikan_t = {
  kiten : string;
  shuten : string;
  keiyu : string;
  kyori : float;
  jikan : int;
}

let rec assoc ekimei lst = match lst with
    [] -> raise Not_found
  | (name, kyori) :: rest -> if ekimei = name then kyori
                                              else assoc ekimei rest

let bulk_insert_ekikan tree lst =
  (* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
  let insert_ekikan tree ekikan =
    (* insert1 : ekikan_tree_t -> string -> string -> float -> ekikan_tree_t *)
    let insert1 tree kiten shuten kyori =
      let lst =
        try RedBlackTree.search tree kiten
        with Not_found -> [] in
      RedBlackTree.insert tree kiten ((shuten, kyori) :: lst) in
    match ekikan with
      {kiten=k; shuten=s; keiyu=_; kyori=d; jikan=_} ->
        insert1 (insert1 tree k s d) s k d in
  List.fold_left insert_ekikan tree lst
