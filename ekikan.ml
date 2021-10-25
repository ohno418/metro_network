(* 駅間type *)
type ekikan_t = {
  kiten : string;  (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string;  (* 経由駅の駅名 *)
  kyori : float;   (* 距離 *)
  jikan : int;     (* 所要時間 *)
}
