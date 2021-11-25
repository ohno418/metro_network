open Ekimei
open Ekikan
open GlobalDataList

exception No_such_station of string

let validate_eki_kanji eki =
  let rec validate_from_global_list kanji lst = match lst with
      [] -> raise (No_such_station (kanji))
    | {kanji=k; kana=_; romaji=_; shozoku=_} :: rest ->
          if k = kanji then ()
                       else validate_from_global_list kanji rest in
  validate_from_global_list eki global_ekimei_list

let get_ekikan_kyori eki1 eki2 tree = assoc eki2 (RedBlackTree.search tree eki1)

(*
let ekikan_tree = bulk_insert_ekikan RedBlackTree.empty global_ekikan_list
let test1 = get_ekikan_kyori "新大塚" "茗荷谷" ekikan_tree = 1.2
let test2 = get_ekikan_kyori "茗荷谷" "新大塚" ekikan_tree = 1.2
let test3 = get_ekikan_kyori "本郷三丁目" "御茶ノ水" ekikan_tree = 0.8
*)
