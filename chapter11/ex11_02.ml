(** 練習問題11.2 *)
(* ファンクター MakeAbstractSet を以下の SET シグネチャで実装せよ *)
(* 出来るだけ多くの例を作って機能を試せ *)

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

module MakeAbstractSet (Order : OrderedType) :
  SET with type elt = Order.t
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

module IntSet = MakeAbstractSet (
  struct
    type t = int
    let compare i j = i - j
  end
)


open IntSet;;

let (<<) s elt = IntSet.add elt s

let s1 = 
  empty << 0 << 1 << 2 << 4 << 7 << 11 << 12

let s2 =
  empty << 1 << 3 << 5 << 7 << 9 << 11 << 13

(* inter s1 s2 *)
let s3 =
  empty << 1 << 7 << 11

let check1 = s3 = inter s1 s2

(* union s1 s2 *)
let s4 =
  empty << 0 << 1 << 2 << 3 << 4 << 5 << 7 << 9 << 11 << 12 << 13

let check2 = s4 = union s1 s2

(* diff s1 s2 *)
let s5 =
  empty << 0 << 2 << 4 << 12

let check3 = s5 = diff s1 s2

(* mem *)
let check4 = (mem 0 s1, mem 3 s1, mem 2200 s2) = (true, false, false)