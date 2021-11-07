(* 最短経路を計算する際に使うデータ集合を扱うための型. *)
type eki_t = {
  namae : string;           (* 駅名 (漢字) *)
  saitan_kyori : float;     (* 最短距離 *)
  temae_list : string list; (* 経路の駅名 (漢字) のリスト *)
}

(* global_ekimei_list から eki_t のリストを生成する. *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list ekimei_lst = match ekimei_lst with
    [] -> []
  | {kanji=k; kana=_; romaji=_; shozoku=_} :: rest ->
        {namae=k; saitan_kyori=infinity; temae_list=[]} :: make_eki_list rest

let test1 = make_eki_list [
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myougadani"; shozoku="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongousanchoume"; shozoku="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"};
] = [
  {namae="茗荷谷"; saitan_kyori=infinity; temae_list=[]};
  {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
  {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
  {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
]


(* eki_t のリストを初期化する.
   始点の駅名 (漢字) を受け取り, 以下の処理をする.
   - saitan_kyori を 0 に
   - temae_list を始点の駅名のみのリストに *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst shiten = match lst with
    [] -> []
  | {namae=n; saitan_kyori=s; temae_list=t} :: rest ->
        if n = shiten then {namae=n; saitan_kyori=0.0; temae_list=[n]} :: rest
                       else {namae=n; saitan_kyori=s; temae_list=t} :: shokika rest shiten

let test2 = shokika [
  {namae="茗荷谷"; saitan_kyori=infinity; temae_list=[]};
  {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
  {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
  {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
] "後楽園" = [
  {namae="茗荷谷"; saitan_kyori=infinity; temae_list=[]};
  {namae="後楽園"; saitan_kyori=0.0; temae_list=["後楽園"]};
  {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
  {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
]


(* insert : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec insert lst e = match lst with
    [] -> [e]
  | first :: rest ->
        if first.kana = e.kana then insert rest e
        else if first.kana < e.kana then first :: insert rest e
        else e :: lst

let test3 = insert [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
] {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"} = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
]
let test4 = insert [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
] {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"} = [
  {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
  {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
]

(* ekimei_t のリストを受け取り, kana でソートして, 重複する駅名のものを削除する. *)
(* seiretsu : ekimei_t list -> ekimei_t *)
let rec seiretsu ekimei_lst = match ekimei_lst with
    [] -> []
  | first :: rest -> insert (seiretsu rest) first

let test5 = seiretsu [
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

(* 直前に確定した駅 p : eki_t と, 未確定の駅 q : eki_t を受け取り,
   p と q が直接つながっているかを調べ,
   つながっていたら q の最短距離と手前リストを必要に応じて更新する.
   つながっていなかったら q をそのまま返す. *)
(* koushin1 : eki_t -> eki_t -> eki_t *)
let koushin1 p q = match (p, q) with
  (
    {namae=n0; saitan_kyori=s0; temae_list=t0},
    {namae=n1; saitan_kyori=s1; temae_list=t1}
  ) ->
    let kyori = (get_ekikan_kyori n0 n1 global_ekikan_list) +. s0 in
      if kyori = infinity
        then q (* 直接つながっていない *)
        else
          if kyori < s1
            then {namae=n1; saitan_kyori=kyori; temae_list=n1::t0} (* 更新 *)
            else q                                                 (* 更新せず *)

let test6 = koushin1
  {namae="茗荷谷"; saitan_kyori=0.0; temae_list=["茗荷谷"]}
  {namae="後楽園"; saitan_kyori=infinity; temae_list=[]}
  = {namae="後楽園"; saitan_kyori=1.8; temae_list=["後楽園"; "茗荷谷"]}
let test7 = koushin1
  {namae="茗荷谷"; saitan_kyori=0.0; temae_list=["茗荷谷"]}
  {namae="後楽園"; saitan_kyori=3.0; temae_list=["後楽園"; "xx"]}
  = {namae="後楽園"; saitan_kyori=1.8; temae_list=["後楽園"; "茗荷谷"]}
let test8 = koushin1
  {namae="茗荷谷"; saitan_kyori=0.0; temae_list=["茗荷谷"]}
  {namae="後楽園"; saitan_kyori=1.0; temae_list=["後楽園"; "xx"]}
  = {namae="後楽園"; saitan_kyori=1.0; temae_list=["後楽園"; "xx"]}
let test9 = koushin1
  {namae="茗荷谷"; saitan_kyori=0.0; temae_list=["茗荷谷"]}
  {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=["後楽園"; "xx"]}
  = {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=["後楽園"; "xx"]}
let test10 = koushin1
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  {namae="後楽園"; saitan_kyori=3.0; temae_list=["後楽園"; "xx"]}
  = {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]}
let test11 = koushin1
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  {namae="後楽園"; saitan_kyori=1.5; temae_list=["後楽園"; "xx"]}
  = {namae="後楽園"; saitan_kyori=1.5; temae_list=["後楽園"; "xx"]}

(* 直前に確定した駅 p : eki_t と未確定の駅リスト v : eki_t list を受け取り,
   必要な更新処理を実行した後の未確定の駅リストを返す. *)
(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p lst = List.map (koushin1 p) lst

let test12 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  []
= []
let test13 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=infinity; temae_list=[]};
    {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
= [
    {namae="新大塚"; saitan_kyori=2.2; temae_list=["新大塚"; "茗荷谷"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
let test14 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=infinity; temae_list=[]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
= [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
let test15 = koushin
  {namae="茗荷谷"; saitan_kyori=1.0; temae_list=["茗荷谷"]}
  [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=3.0; temae_list=["後楽園"; "yyy"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
= [
    {namae="新大塚"; saitan_kyori=2.0; temae_list=["新大塚"; "xxx"]};
    {namae="後楽園"; saitan_kyori=2.8; temae_list=["後楽園"; "茗荷谷"]};
    {namae="本郷三丁目"; saitan_kyori=infinity; temae_list=[]};
    {namae="御茶ノ水"; saitan_kyori=infinity; temae_list=[]};
  ]
