open Ekimei
open Ekikan
open GlobalDataList

(* ekimei_tを受け取り、「路線名, 駅名 (かな)」の文字列返す *)
(* hyouji : ekimei_t -> string *)
(*
let hyouji ekimei = match ekimei with
  {kanji = a; kana = b; romaji = _; shozoku = d} -> d ^ ", " ^ a ^ " (" ^ b ^ ")"

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
*)


exception No_such_station of string

(* ローマ字の駅名 romaji と駅名リストを受け取り漢字の駅名を返す.
   見つからない場合は No_such_station を raise する. *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
(*
let rec romaji_to_kanji romaji lst = match lst with
    [] -> raise (No_such_station (romaji))
  | {kanji=k; kana=_; romaji=r; shozoku=_} :: rest ->
        if r = romaji then k
                      else romaji_to_kanji romaji rest

let test4 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let test5 = romaji_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原"
let test6 = romaji_to_kanji "shin-ochanomizu" global_ekimei_list = "新御茶ノ水"
*)

(* 漢字の駅名 romaji と駅名リストを受け取り, ローマ字の駅名を返す.
   見つからない場合は No_such_station を raise する. *)
(*
let rec kanji_to_romaji kanji lst = match lst with
    [] -> raise (No_such_station (kanji))
  | {kanji=k; kana=_; romaji=r; shozoku=_} :: rest ->
        if k = kanji then r
                     else kanji_to_romaji kanji rest
*)

(* 漢字の駅名と ekimei_t のリストを受け取り, 存在する駅名の場合は unit を返す.
   存在しない駅名の場合は No_such_station を raise する. *)
let validate_eki_kanji eki =
  let rec validate_from_global_list kanji lst = match lst with
      [] -> raise (No_such_station (kanji))
    | {kanji=k; kana=_; romaji=_; shozoku=_} :: rest ->
          if k = kanji then ()
                       else validate_from_global_list kanji rest in
  validate_from_global_list eki global_ekimei_list


(* 駅名 (漢字) 2つと駅間treeを受け取り駅間距離を返す.
   直接つながっていない駅同士の場合は Not_found を raise する. *)
(* get_ekikan_kyori : string -> string -> (string, (string * float) list) RedBlackTree.t -> float *)
let get_ekikan_kyori eki1 eki2 tree = assoc eki2 (RedBlackTree.search tree eki1)

(*
let ekikan_tree = bulk_insert_ekikan RedBlackTree.empty global_ekikan_list
let test7 = get_ekikan_kyori "新大塚" "茗荷谷" ekikan_tree = 1.2
let test8 = get_ekikan_kyori "茗荷谷" "新大塚" ekikan_tree = 1.2
let test9 = get_ekikan_kyori "本郷三丁目" "御茶ノ水" ekikan_tree = 0.8
*)


(* ローマ字の駅名2つを受け取り, "茗荷谷駅から新大塚駅までは1.2kmです" という文字列を返す.
   直接つながっていない駅同士は "茗荷谷駅と本郷三丁目駅はつながっていません" という文字列を返す.
   存在しない駅名については "asdfという駅名は存在しません" という文字列を返す. *)
(* kyori_wo_hyoji : string -> string -> string *)
(*
let kyori_wo_hyoji r_eki1 r_eki2 =
    let k_eki1 = romaji_to_kanji r_eki1 global_ekimei_list in
    let k_eki2 = romaji_to_kanji r_eki2 global_ekimei_list in
        if k_eki1 = "" then r_eki1 ^ "という駅名は存在しません" else
        if k_eki2 = "" then r_eki2 ^ "という駅名は存在しません" else
            let kyori = get_ekikan_kyori k_eki1 k_eki2 (bulk_insert_ekikan RedBlackTree.empty global_ekikan_list) in
                if kyori = infinity then k_eki1 ^ "駅と" ^ k_eki2 ^ "駅はつながっていません"
                                    else k_eki1 ^ "駅から" ^ k_eki2 ^ "駅までは" ^
                                         string_of_float kyori ^ "kmです"

let test12 = kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです"
let test13 = kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚駅から茗荷谷駅までは1.2kmです"
(*
let test14 = kyori_wo_hyoji "abc" "myogadani" = "abcという駅名は存在しません"
let test15 = kyori_wo_hyoji "myogadani" "abc" = "abcという駅名は存在しません"
let test16 = kyori_wo_hyoji "abc" "def" = "abcという駅名は存在しません"
let test17 = kyori_wo_hyoji "myogadani" "hongosanchome" = "茗荷谷駅と本郷三丁目駅はつながっていません"
*)
let test18 = kyori_wo_hyoji "ochanomizu" "hongosanchome" = "御茶ノ水駅から本郷三丁目駅までは0.8kmです"
*)
