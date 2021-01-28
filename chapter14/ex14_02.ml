(** 練習問題14.2 *)
(* 拡張リストを単なるリストに変換する関数 list_of_alist を定義せよ。 *)
let l1 = `Nil
let l2 = `Cons(1, `Nil)
let l3 = `Cons(2, `Cons(3, `Nil))
let l4 = `Cons(5, `Cons(10, `Cons(15, `Cons(20, `Nil))))
let l5 = `Cons(7, `Cons(14, `Cons(21, `Cons(28, `Nil))))

let make_append f l1 l2 =
  match (l1, l2) with
  | (`Nil, l2) -> l2
  | (`Cons(x, rest), l2) -> `Cons(x, f rest l2)

let rec append l1 l2 = make_append append l1 l2

let rec list_of_alist = function
  | `Nil -> `Nil
  | `Cons(x, rest) -> `Cons(x, list_of_alist rest)
  | `App(l1, l2) -> append (list_of_alist l1) (list_of_alist l2)

let l6 = `Cons(0, `App(l4, l5))