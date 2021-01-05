(* 練習問題5.5 *)
let rec fold_right f l e =
  match l with
    | [] -> e
    | x :: rest -> f x (fold_right f rest e)

let concat l1 l2 =
  fold_right
    (fun a e -> a :: e) l1 l2

let l1 = [1; 2; 3]
and l2 = [4; 5; 6]

let check1 = concat l1 l2

let forall f l =
  fold_right (fun a e -> f a && e) l true

let check2 = forall (fun x -> x > 0) [1; 2; 3; 4; 6]
let check3 = forall (fun x -> x > 0) [1; 2; 3; 4; -4]

let exists f l =
  fold_right (fun a e -> f a || e) l false

let check4 = exists (fun x -> x > 0) [-1; 0; -3; -4]
let check5 = exists (fun x -> x > 0) [-1; -3; -4; 4]