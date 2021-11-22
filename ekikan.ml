(* 駅間type *)
type ekikan_t = {
  kiten : string;  (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string;  (* 経由駅の駅名 *)
  kyori : float;   (* 距離 *)
  jikan : int;     (* 所要時間 *)
}


(* 駅間データを収めたtree *)
(* Node : 駅名 (漢字) * (直接つながっている駅 * 距離) list *)
type ekikan_tree_t = (string, (string * float) list) Tree.t


(* - 駅名 (漢字) : string
   - 駅名と距離の組のリスト : (string * float) list
   を受け取り, 距離を返す.
   駅名が見つからない場合は Not_found を raise する. *)
(* assoc : 'a -> ('a * 'b) list -> 'b *)
let rec assoc ekimei lst = match lst with
    [] -> raise Not_found
  | (name, kyori) :: rest -> if ekimei = name then kyori
                                              else assoc ekimei rest



(* Bulk insert. *)
(* bulk_insert_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let bulk_insert_ekikan tree lst =
  (* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
  let insert_ekikan tree ekikan =
    (* insert1 : ekikan_tree_t -> string -> string -> float -> ekikan_tree_t *)
    let insert1 tree kiten shuten kyori =
      let lst =
        try Tree.search tree kiten
        with Not_found -> [] in
      Tree.insert tree kiten ((shuten, kyori) :: lst) in
    match ekikan with
      {kiten=k; shuten=s; keiyu=_; kyori=d; jikan=_} ->
        insert1 (insert1 tree k s d) s k d in
  List.fold_left insert_ekikan tree lst
