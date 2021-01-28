let l1 = `Nil
let l2 = `Cons(1, `Nil)
let l3 = `Cons(2, `Cons(3, `Nil))
let l4 = `Cons(5, `Cons(10, `Cons(15, `Cons(20, `Nil))))
let l5 = `Cons(7, `Cons(14, `Cons(21, `Cons(28, `Nil))))
let l6 = `Cons(0, `App(l4, l5))

let rec max_list = function
  | `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons(y, rest) as l)) ->
      let m = max_list l in
      if x > m then x else m

let rec max_list' = function
  | `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons(y, rest) as l)) ->
      let m = max_list' l in
      max x m

(*
  この 2 つは同じことをしているように見えるので、
  例えば max_list' を呼び出すところを max_list
  にしても良さそう。でも実際は型が変わっちゃう。なんで？
  これ見た目は違うけど実質は同じかもしれない
*)

let rec max_list'' = function
  | `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons(y, rest) as l)) ->
      let m = max_list l in
      max x m


let rec amax_list' = function
  | `Cons (x, `Nil) -> x
  | `Cons (x, (`Cons(y, rest) as l)) ->
      let m = amax_list' l in
      max x m
  | `Cons (x, (`App(l1, l2) as l)) ->
      let m = amax_list' l in
      max x m
  | `App(l1, l2) ->
      max (amax_list' l1) (amax_list' l2)

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