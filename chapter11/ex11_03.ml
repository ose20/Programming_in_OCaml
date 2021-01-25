(** 練習問題11.3 *)
(* 
  MakeAbstractSet を次のように、ファンクターの返すモジュールのシグネチャに
  with をつけず SET だけにして定義するとどんな不都合があるか？
*)

module type OrderedType =
  sig
    type t
    (* compare i j は i < j なら負、i = j なら 0、i > j なら正の値 *)
    val compare : t -> t -> int
  end

module type SET =
  sig
    type elt
    type t
    val empty : t                 (* 空集合 *)
    val is_empty : t -> bool      (* 集合か空かのテスト *)
    val mem : elt -> t -> bool    (* elt が t に属しているかのテスト *)
    val add : elt -> t -> t       (* 要素 elt を t に加えた集合を返す *)
    val inter : t -> t -> t       (* ふたつの集合の共通部分を返す *)
    val union : t -> t -> t       (* ふたつの集合の和集合を返す *)
    val diff : t -> t -> t        (* ふたつの集合の差を返す *)
    val elements : t -> elt list  (* 集合要素を昇順整列済みリストとして返す *)
  end

module MakeAbstractSet (Order : OrderedType) : SET
=
  struct
    type elt = Order.t
    type t = elt list
    
    let empty = []

    let is_empty = function
      | [] -> true
      | _ -> false

    let rec mem elt = function
      | [] -> false
      | x :: rest ->
          match Order.compare elt x with
          | 0 -> true
          | r when r < 0 -> false
          | _ -> mem elt rest

    let rec add elt = function
      | [] -> [elt]
      | (x :: rest as s) ->
          match Order.compare elt x with
          | 0 -> s
          | r when r < 0 -> elt :: s
          | _ -> x :: (add elt rest)

    let rec inter s1 s2 =
      match (s1, s2) with
      | ([], _) -> []
      | (_, []) -> []
      | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
          match Order.compare e1 e2 with
          | 0 -> e1 :: (inter rest1 rest2)
          | r when r < 0 -> inter rest1 s2
          | _ -> inter s1 rest2

    let rec union s1 s2 =
      match (s1, s2) with
      | ([], s2) -> s2
      | (s1, []) -> s1
      | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
          match Order.compare e1 e2 with
          | 0 -> e1 :: (union rest1 rest2)
          | r when r < 0 -> e1 :: (union rest1 s2)
          | _ -> e2 :: (union s1 rest2)

    let rec diff s1 s2 =
      match (s1, s2) with
      | ([], _) -> []
      | (s1, []) -> s1
      | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
          match Order.compare e1 e2 with
          | 0 -> diff rest1 rest2
          | r when r < 0 -> e1 :: (diff rest1 s2)
          | _ -> diff s1 rest2

    let elements s = s
  end

(*
  このように定義した場合、elt の中身が隠蔽されているので
  例えば次のコードがエラーになる。  
*)

module IntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end
)

open IntSet

(* この次のコードでエラー *)
(* let s1 = add 1 empty *)
(*
  集合を作るには本質的に add に頼るしかないが、
  そこで必要になる IntSet.elt は隠蔽されていて外から与えることができないので
  結局このモジュールで集合を作ること自体が不可能になってしまう...
*)