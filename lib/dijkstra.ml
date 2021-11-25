open Ekimei
open Ekikan
open RedBlackTree
open GlobalDataList
open Utils

(* 最短経路を計算する際に使うデータ集合を扱うための型. *)
type eki_t = {
  namae : string;           (* 駅名 (漢字) *)
  saitan_kyori : float;     (* 最短距離 *)
  temae_list : string list; (* 経路の駅名 (漢字) のリスト *)
}


(* 駅名のリスト lst : ekimei_t list と, 始点の駅名 shiten : string を受け取り,
   初期化した eki_t のリストを返す.

   初期化とは, 始点の eki_t について以下の処理を行うことを指す.
   - saitan_kyori を 0 に.
   - temae_list を始点の駅名のみのリストに. *)
(* make_eki_list : ekimei_t list -> string -> eki_t list *)
let make_initial_eki_list lst shiten =
  List.map
    (fun eki -> match eki with
      {kanji=k; kana=_; romaji=_; shozoku=_} ->
        if k = shiten then {namae=k; saitan_kyori=0.0; temae_list=[k]}
                      else {namae=k; saitan_kyori=infinity; temae_list=[]})
    lst

(*
let test1 = make_initial_eki_list [
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myougadani"; shozoku="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"};
] "後楽園"
= [
  {namae="茗荷谷"; saitan_kyori=infinity; temae_list=[]};
  {namae="後楽園"; saitan_kyori=0.0; temae_list=["後楽園"]};
  {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
  {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
]
*)


(* insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec insert lst e = match lst with
    [] -> [e]
  | first :: rest ->
        if first.kana = e.kana then insert rest e
        else if first.kana < e.kana then first :: insert rest e
        else e :: lst

(*
let test2 = insert [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
] {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"} = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
]
let test3 = insert [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
] {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"} = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
]
*)

(* ekimei_t のリストを受け取り, kana でソートして, 重複する駅名のものを削除する. *)
(* seiretsu : ekimei_t list -> ekimei_t *)
let rec seiretsu ekimei_lst = match ekimei_lst with
    [] -> []
  | first :: rest -> insert (seiretsu rest) first

(*
let test4 = seiretsu [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
  {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
] = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
]
*)


(* - 直前に確定した駅 p : eki_t
   - 未確定の駅リスト v : eki_t list
   - 駅間データ : ekikan_t list
   を受け取り, 必要な更新処理を実行した後の未確定の駅リストを返す. *)
(* koushin : eki_t -> eki_t list -> ekikan_tree -> eki_t list *)
let koushin p eki_list ekikan_tree =
  List.map
    (* 未確定の駅 q : eki_t を受け取り,
       確定済みの駅 p : eki_t と q が直接つながっているかを調べ,
       つながっていたら q の最短距離と手前リストを必要に応じて更新する.
       つながっていなかったら q をそのまま返す. *)
    (fun q -> match (p, q) with
      (
        {namae=n0; saitan_kyori=s0; temae_list=t0},
        {namae=n1; saitan_kyori=s1; temae_list=_}
      ) ->
        try
          let kyori = (get_ekikan_kyori n0 n1 ekikan_tree) +. s0 in
          if kyori < s1
            then {namae=n1; saitan_kyori=kyori; temae_list=n1::t0}
            else q
        with Not_found -> q)
    eki_list

(*
let ekikan_tree = bulk_insert_ekikan empty global_ekikan_list
let test5 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [] ekikan_tree
= []
let test6 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=infinity; temae_list=[]};
    {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ] ekikan_tree
= [
    {namae="新大塚"; saitan_kyori=2.2; temae_list=["新大塚"; "茗荷谷"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
let test7 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ] ekikan_tree
= [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
let test8 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=3.0; temae_list=["後楽園"; "yyy"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ] ekikan_tree
= [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
*)


(* eki_t のリストを受け取り,
   最短距離最小の駅(Option) と それ以外の駅のリスト を返す. *)
(* saitan_wo_bunri : eki_t list -> eki_t option * eki_t list *)
let saitan_wo_bunri lst = List.fold_right
  (fun item (saitan_op, rest) ->
    match (item, saitan_op) with
        ({namae=_; saitan_kyori=_; temae_list=_}, None) -> (Some item, rest)
      | ({namae=_; saitan_kyori=s0; temae_list=_},
         Some {namae=_; saitan_kyori=s1; temae_list=_}) ->
           if s0 < s1 then (Some item, (Option.get saitan_op) :: rest)
                      else (saitan_op, item :: rest))
  lst
  (None, [])

(*
let test9 = saitan_wo_bunri [
  {namae="新大塚"; saitan_kyori=1.0; temae_list=[]};
  {namae="後楽園"; saitan_kyori=0.4; temae_list=[]};
  {namae="本郷三丁目"; saitan_kyori=1.5; temae_list=[]};
  {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
] = (
  Some {namae="後楽園"; saitan_kyori=0.4; temae_list=[]},
  [
    {namae="新大塚"; saitan_kyori=1.0; temae_list=[]};
    {namae="本郷三丁目"; saitan_kyori=1.5; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
)
*)


(* 未確定の駅のリスト eki_list と, 駅間データ ekikan_list を受け取り,
   ダイクストラのアルゴリズムにより,
   各駅について最短距離と最短経路が正しく入ったリストを返す. *)
(* dijkstra_main : eki_t list -> ekikan_tree -> eki_t list *)
let rec dijkstra_main eki_list ekikan_tree = match eki_list with
    [] -> []
  | {namae=_; saitan_kyori=_; temae_list=_} :: _ ->
      let (saitan_op, rest) = saitan_wo_bunri eki_list in
      let saitan = Option.get saitan_op in
      let undetermined_eki_list = koushin saitan rest ekikan_tree in
      saitan :: dijkstra_main undetermined_eki_list ekikan_tree


(* dijkstra : string -> string -> eki_t *)
let dijkstra shiten_kanji shuten_kanji =
  let init_list = make_initial_eki_list (seiretsu global_ekimei_list) shiten_kanji in
  let ekikan_tree = bulk_insert_ekikan empty global_ekikan_list in
  let result_list = dijkstra_main init_list ekikan_tree in
  let rec find_shuten lst shuten = match lst with
      [] -> failwith "shuten not found"
    | {namae=n; saitan_kyori=_; temae_list=_} as first :: rest ->
        if n = shuten then first else find_shuten rest shuten in
  find_shuten result_list shuten_kanji


(*
let test10 = dijkstra "myogadani" "hongosanchome"
  = {namae="本郷三丁目"; saitan_kyori=2.6; temae_list=["本郷三丁目"; "後楽園"; "茗荷谷"]}
let test11 = (dijkstra "myogadani" "shinjuku-gyoemmae").namae = "新宿御苑前"
let test12 = (dijkstra "myogadani" "shinjuku-gyoemmae").temae_list = ["新宿御苑前"; "四谷三丁目"; "四ツ谷"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]
*)


(* eki_t を受け取り, 経由を含めた最短距離を説明する. *)
(* print_eki : eki_t -> unit *)
let print_eki eki = match eki with
  {namae=n; saitan_kyori=s; temae_list=lst} ->
    (print_string n;
     print_string "へは, ";
     print_string (List.fold_right
       (fun e str -> if str = "" then e else str ^ "->" ^ e)
       lst "");
     print_string "という順番で経由し, ";
     print_float s;
     print_string "kmです.";
     print_newline ())
