val romaji_to_kanji : string -> Ekimei.ekimei_t list -> string
  (* ローマ字の駅名 romaji と駅名リストを受け取り漢字の駅名を返す.
     見つからない場合は No_such_station を raise する. *)

val get_ekikan_kyori : string -> string -> (string, (string * float) list) RedBlackTree.t -> float
  (* 駅名 (漢字) 2つと駅間treeを受け取り駅間距離を返す.
     直接つながっていない駅同士の場合は Not_found を raise する. *)
