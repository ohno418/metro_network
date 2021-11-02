(* 最短経路を計算する際に使うデータ集合を扱うための型. *)
type eki_t = {
  namae : string;       (* 駅名 (漢字) *)
  saitan_kyori : float; (* 最短距離 *)
  temae_list : string list;  (* 経路の駅名 (漢字) のリスト *)
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
