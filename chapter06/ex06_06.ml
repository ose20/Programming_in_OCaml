(* 練習問題6.6 *)
type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let preord t =
  let rec iter t rest =
    match t with
        Lf -> rest
      | Br(x, left, right) -> x :: (iter left (iter right rest))
  in iter t []


let t1 = Br(1,  Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)),
                Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)))

let check1 = preord t1

let inord t =
  let rec iter t rest =
    match t with
        Lf -> rest
      | Br(x, left, right) -> iter left (x :: iter right rest)
  in 
  iter t []

let check2 = inord t1

let postord t =
  let rec iter t rest =
    match t with
        Lf -> rest
      | Br(x, left, right) -> iter left @@ iter right (x :: rest)
  in iter t []

let check3 = postord t1