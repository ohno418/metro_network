(* 駅間type *)
type ekikan_t = {
  kiten : string;  (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string;  (* 経由駅の駅名 *)
  kyori : float;   (* 距離 *)
  jikan : int;     (* 所要時間 *)
}

val assoc : 'a -> ('a * 'b) list -> 'b
  (* - 駅名 (漢字) : string
     - 駅名と距離の組のリスト : (string * float) list
     を受け取り, 距離を返す.
     駅名が見つからない場合は Not_found を raise する. *)

val bulk_insert_ekikan : (string, (string * float) list) RedBlackTree.t -> ekikan_t list -> (string, (string * float) list) RedBlackTree.t
  (* Bulk insert ekikans into tree. *)
