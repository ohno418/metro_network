(* 駅間type *)
type ekikan_t = {
  kiten : string;  (* 起点の駅名 *)
  shuten : string; (* 終点の駅名 *)
  keiyu : string;  (* 経由駅の駅名 *)
  kyori : float;   (* 距離 *)
  jikan : int;     (* 所要時間 *)
}


(* 駅間データを収めたtree *)
(* Node : 駅名 (漢字) * (直接つながっている駅 * 距離) list *)
type ekikan_tree_t = Empty
                   | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t


(* - 駅名 (漢字) : string
   - 駅名と距離の組のリスト : (string * float) list
   を受け取り, 距離を返す.
   駅名が見つからない場合は Not_found を raise する. *)
(* assoc : 'a -> ('a * 'b) list -> 'b *)
let rec assoc ekimei lst = match lst with
    [] -> raise Not_found
  | (name, kyori) :: rest -> if ekimei = name then kyori
                                              else assoc ekimei rest

let test1 = assoc "茗荷谷" [("茗荷谷", 1.2); ("後楽園", 0.8)] = 1.2


(* ekikan を ekikan_tree に挿入する. *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan tree ekikan =
  (* 1つだけ駅間データを木に追加する. *)
  (* insert1 : ekikan_tree -> string -> string -> float -> ekikan_tree *)
  let rec insert1 tree kiten shuten kyori = match tree with
      Empty -> Node (Empty, kiten, [(shuten, kyori)], Empty)
    | Node (t1, ekimei, lst, t2) ->
        if kiten < ekimei then Node (insert1 t1 kiten shuten kyori, ekimei, lst, t2)
        else if kiten > ekimei then Node (t1, ekimei, lst, insert1 t2 kiten shuten kyori)
        else Node (t1, ekimei, (shuten, kyori) :: lst, t2) in
  match ekikan with
    {kiten=k; shuten=s; keiyu=_; kyori=d; jikan=_} -> insert1 (insert1 tree k s d) s k d

(* test examples *)
let ekikan1 = {kiten="湯島"; shuten="根津"; keiyu="千代田線"; kyori=1.2; jikan=2}
let ekikan2 = {kiten="根津"; shuten="千駄木"; keiyu="千代田線"; kyori=1.0; jikan=2}
let ekikan3 = {kiten="千駄木"; shuten="西日暮里"; keiyu="千代田線"; kyori=0.9; jikan=1}

let ekikan_tree1 = insert_ekikan Empty ekikan1
let ekikan_tree2 = insert_ekikan ekikan_tree1 ekikan2
let ekikan_tree3 = insert_ekikan ekikan_tree2 ekikan3
let test2 = ekikan_tree1 = Node (Node (Empty, "根津", [("湯島", 1.2)], Empty), "湯島", [("根津", 1.2)], Empty)
let test3 = ekikan_tree2 = Node (Node (Node (Empty, "千駄木", [("根津", 1.0)], Empty), "根津", [("千駄木", 1.0); ("湯島", 1.2)], Empty), "湯島", [("根津", 1.2)], Empty)
let test4 = ekikan_tree3 = Node (Node (Node (Empty, "千駄木", [("西日暮里", 0.9); ("根津", 1.0)], Empty), "根津", [("千駄木", 1.0); ("湯島", 1.2)], Empty), "湯島", [("根津", 1.2)], Node (Empty, "西日暮里", [("千駄木", 0.9)], Empty))


(* Buld insert. *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan tree lst = List.fold_left insert_ekikan tree lst

let test5 = inserts_ekikan Empty [ekikan1; ekikan2; ekikan3] = Node (Node (Node (Empty, "千駄木", [("西日暮里", 0.9); ("根津", 1.0)], Empty), "根津", [("千駄木", 1.0); ("湯島", 1.2)], Empty), "湯島", [("根津", 1.2)], Node (Empty, "西日暮里", [("千駄木", 0.9)], Empty))
