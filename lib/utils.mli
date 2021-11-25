val validate_eki_kanji : string -> unit
  (* 漢字の駅名を受け取り, global_ekimei_list に存在する駅名の場合は unit を返す.
     存在しない駅名の場合は No_such_station を raise する. *)

val get_ekikan_kyori : string -> string -> (string, (string * float) list) RedBlackTree.t -> float
  (* 駅名 (漢字) 2つと駅間treeを受け取り駅間距離を返す.
     直接つながっていない駅同士の場合は Not_found を raise する. *)
