type eki_t

val dijkstra : string -> string -> eki_t
  (* 始点の駅名 (漢字) と, 終点の駅名 (漢字) を受け取り,
     終点の eki_t を返す. *)

val print_eki : eki_t -> unit
  (* eki_t を受け取り, 経由を含めた最短距離を説明する. *)
