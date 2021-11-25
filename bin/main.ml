open Metro_network.Dijkstra
open Metro_network.Utils

(* 始点の駅名 (漢字) と終点の駅名 (漢字) を受け取り,
   最短経路での行き方をprintする. *)
let main shiten shuten =
  (validate_eki_kanji shiten;
   validate_eki_kanji shuten;
   let eki = dijkstra shiten shuten in
   print_eki eki)

let _ = main (Sys.argv.(1)) (Sys.argv.(2))
