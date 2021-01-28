(** 練習問題14.3 *)
let l1 = `Nil
let l2 = `Cons(1, `Nil)
let l3 = `Cons(2, `Cons(3, `Nil))
let l4 = `Cons(5, `Cons(10, `Cons(15, `Cons(20, `Nil))))
let l5 = `Cons(7, `Cons(14, `Cons(21, `Cons(28, `Nil))))
let l6 = `Cons(0, `App(l4, l5))

let l7 = `App(l5, l6)
let l8 = `Cons(-1, l6)

(* max_list の拡張 *)
(* まず max_list を作る *)
let make_max_list f = function
  | `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons(y, rest) as l)) ->
      let m = f l in
      max x m

let rec max_list l = make_max_list max_list l

let make_amax_list f = function
  | `Cons (x, `Nil) | `Cons(x, `Cons(_, _)) as l -> make_max_list f l
  | `Cons (x, (`App(l1, l2) as l)) ->
      let m = f l in max x m
  | `App(l1, l2) -> max (f l1) (f l2)

let rec amax_list l = make_amax_list amax_list l

(* test *)
let f x = 10 * x
let tests = List.map (fun l -> amax_list l) [l4; l5; l6; l7; l8]

(* map の拡張 *)
(* 通常の map *)
let rec map' f = function
  | `Nil -> `Nil
  | `Cons(x, rest) -> `Cons(f x, map' f rest)

let make_map map f = function
  | `Nil -> `Nil
  | `Cons(x, rest) -> `Cons(f x, map f rest)

let rec map f l = make_map map f l

(* 拡張されたリストに対する直接的な map *)
let rec amap' f = function
  | `Nil -> `Nil
  | `Cons(x, rest) -> `Cons(f x, amap' f rest)
  | `App(l1, l2) -> `App(amap' f l1, amap' f l2)

let make_amap amap f = function
  | `Nil | `Cons(_, _) as l -> make_map amap f l
  | `App(l1, l2) -> `App(amap f l1, amap f l2)

let rec amap f l = make_amap amap f l

(* test *)
let f x = x * 10
let tests = List.map (fun l -> amap f l) [l4; l5; l6; l7; l8]