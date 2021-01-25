(** 練習問題11.1 *)
(* 以下で与えられる IntSet と IntSet' の違いを説明せよ *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module OrderedInt' : OrderedType =
  struct
    type t = int
    let compare i j = i - j
  end

module MakeSet (Order : OrderedType) =
  struct
    type elt = Order.t
    type t = elt list

    let empty = []

    let empty = []

    let rec mem elt = function
      | [] -> false
      | x :: rest ->
          let r = Order.compare elt x in
          if r = 0 then true
          else if r < 0 then false
          else mem elt rest

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

      let rec elements s = s
  end

module IntSet = MakeSet (
  struct
    type t = int
    let compare i j = i - j
  end
)

module IntSet' = MakeSet (OrderedInt')

(*
  この時例えば open でモジュールを宣言した後の
  let s1 = add 1 (add 2 empty)
  に対して、IntSet は問題ないが IntSet' では以下の型エラーが生じる。

  # let s1 = add 1 (add 2 empty);; (1 に下線)
  Error: This expression has type int but an expression was 
  expected of type　OrderedInt'.t

  おそらく、MakeSet の段階で、引数のモジュールの情報に関して使えるものが関係している。
  IntSet 方では struct ... end が一緒に定義されているのでそこの範囲を参照できるが、
  IntSet' の方では既に定義されたモジュールを受け取るので、そのシグネチャしか
  参照できない。OrderedType は型 t の中身を隠蔽しているので、ここで int を渡すと
  型エラーが出されるというしくみだと推測する。
*)

(*
  この推察が正しければ、シグネチャを参照したときに隠蔽されていなければいいので、
  以下の定義による IntSet'' では正しく動くはず。実際
  open IntSet''
  let s1 = add 1 (add 2 empty)
  はエラーを吐かない。
*)

module OrderedInt'' =
  struct
    type t = int
    let compare i j = i - j
  end

module IntSet'' = MakeSet (OrderedInt'')