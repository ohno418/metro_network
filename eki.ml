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
