(* ローマ字の駅名 romaji と駅名リストを受け取り漢字の駅名を返す.
   見つからない場合は "" を返す. *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji romaji lst = match lst with
    [] -> ""
  | {kanji=k; kana=_; romaji=r; shozoku=_} :: rest ->
        if r = romaji then k
                      else romaji_to_kanji romaji rest

let test1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test2 = romaji_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原"
let test3 = romaji_to_kanji "shin-ochanomizu" global_ekimei_list = "新御茶ノ水"

(* 漢字の駅名2つと駅間リストを受け取り駅間距離を返す.
   直接つながっていない駅同士の場合は infinity を返す. *)
(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten=k; shuten=s; keiyu=_; kyori=kyori; jikan=_} :: rest ->
        if (k = eki1 && s = eki2) || (k = eki2 && s = eki1)
            then kyori
            else get_ekikan_kyori eki1 eki2 rest

let test4 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2
let test5 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let test6 = get_ekikan_kyori "本郷三丁目" "御茶ノ水" global_ekikan_list = 0.8
let test7 = get_ekikan_kyori "茗荷谷" "本郷三丁目" global_ekikan_list = infinity

(* ローマ字の駅名2つを受け取り, "茗荷谷駅から新大塚駅までは1.2kmです" という文字列を返す.
   直接つながっていない駅同士は "茗荷谷駅と本郷三丁目駅はつながっていません" という文字列を返す.
   存在しない駅名については "asdfという駅名は存在しません" という文字列を返す. *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji r_eki1 r_eki2 =
    let k_eki1 = romaji_to_kanji r_eki1 global_ekimei_list in
    let k_eki2 = romaji_to_kanji r_eki2 global_ekimei_list in
        if k_eki1 = "" then r_eki1 ^ "という駅名は存在しません" else
        if k_eki2 = "" then r_eki2 ^ "という駅名は存在しません" else
            let kyori = get_ekikan_kyori k_eki1 k_eki2 global_ekikan_list in
                if kyori = infinity then k_eki1 ^ "駅と" ^ k_eki2 ^ "駅はつながっていません"
                                    else k_eki1 ^ "駅から" ^ k_eki2 ^ "駅までは" ^
                                         string_of_float kyori ^ "kmです"

let test8 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです"
let test9 = kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚駅から茗荷谷駅までは1.2kmです"
let test10 = kyori_wo_hyoji "abc" "myogadani" = "abcという駅名は存在しません"
let test11 = kyori_wo_hyoji "myogadani" "abc" = "abcという駅名は存在しません"
let test12 = kyori_wo_hyoji "abc" "def" = "abcという駅名は存在しません"
let test13 = kyori_wo_hyoji "myogadani" "hongosanchome" = "茗荷谷駅と本郷三丁目駅はつながっていません"
let test14 = kyori_wo_hyoji "ochanomizu" "hongosanchome" = "御茶ノ水駅から本郷三丁目駅までは0.8kmです"
