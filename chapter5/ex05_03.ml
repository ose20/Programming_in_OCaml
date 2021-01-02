(* 練習問題5.3 *)
(* 1 *)
let rec exist p = function
    [] -> false
  | x :: rest -> if p x then true else exist p rest

let mem a s = exist (fun x -> x = a) s

(* 2 *)
let rec fold_left f e l =
  match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest

let intersect s1 s2 =
  fold_left
    (fun e x -> if mem x s2 then x :: e else e)
    []
    s1

let s1 = [1; 2; 3; 4]
and s2 = [3; 4; 5; 6]

let check8 = intersect s1 s2

(* 3 *)
let union s1 s2 =
  fold_left
    (fun e x -> if mem x s2 then e else x :: e)
    s2
    s1

let check9 = union s1 s2

(* 4 *)
let diff s1 s2 =
  fold_left
    (fun e x -> if mem x s2 then e else x :: e)
    []
    s1
  
let check10 = diff s1 s2