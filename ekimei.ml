(* 駅名type *)
type ekimei_t = {
  kanji : string;   (* 漢字の駅名 *)
  kana : string;    (* ひらがなの駅名 *)
  romaji : string;  (* ローマ字の駅名 *)
  shozoku : string; (* 路線名 *)
}

(* ekimei_tを受け取り、「路線名, 駅名 (かな)」の文字列返す *)
(* hyouji : ekimei_t -> string *)
let hyouji ekimei = match ekimei with
  {kanji = a; kana = b; romaji = c; shozoku = d} -> d ^ ", " ^ a ^ " (" ^ b ^ ")"

let test1 = hyouji {kanji = "茗荷谷";
                    kana = "みょうがだに";
                    romaji = "myougadani";
                    shozoku = "丸ノ内線"}
          = "丸ノ内線, 茗荷谷 (みょうがだに)"
let test2 = hyouji {kanji = "本郷三丁目";
                    kana = "ほんごうさんちょうめ";
                    romaji = "hongousanchoume";
                    shozoku = "丸ノ内線"}
          = "丸ノ内線, 本郷三丁目 (ほんごうさんちょうめ)"
let test3 = hyouji {kanji = "新宿";
                    kana = "しんじゅく";
                    romaji = "shinjuku";
                    shozoku = "山手線"}
          = "山手線, 新宿 (しんじゅく)"
