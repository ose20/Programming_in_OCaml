(** 練習問題14.1 *)
(*
  `Nil と `Cons を使って表現されるリストに対して、
  applend, map, downto1 関数を定義せよ。
*)

let l1 = `Nil
let l2 = `Cons(1, `Nil)
let l3 = `Cons(2, `Cons(3, `Nil))
let l4 = `Cons(5, `Cons(10, `Cons(15, `Cons(20, `Nil))))
let l5 = `Cons(true, `Nil)

let rec append l1 l2 =
  match (l1, l2) with
  | (`Nil, l2) -> l2
  | (`Cons(x, rest), l2) -> `Cons(x, append rest l2)

let check1 = append l1 l2
let check2 = append l2 l3
let check3 = append l4 l3
(* type error *)
(* let check = append l2 l4 *)

let rec map f = function
  | `Nil -> `Nil
  | `Cons(x, rest) -> `Cons(f x, map f rest)

let check4 = map (fun x -> x * 10) l4
let check5 = map (fun x -> x * 10) l1

let rec downto1 n =
  if n <= 0 then `Nil
  else `Cons(n, downto1 (n - 1))

let check6 = downto1 5