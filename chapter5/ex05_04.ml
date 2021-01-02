(* 練習問題5.4 *)
(* map f (map g l) を map を一度しか使わないように書き換える *)
let rec map f l =
  match l with
      [] -> []
    | x :: rest -> f x :: map f rest

let ($) f g x = f (g x)

let composite_map f g l = map (($) f g) l

let l1 = ['a'; 'b'; 'c'; 'A']
let check1 = composite_map char_of_int int_of_char l1