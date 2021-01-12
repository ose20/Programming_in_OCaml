(* 練習問題6.12 *)
(*  同じ順番でノードを追加すると必ず同じ木になるので
    4! 通りすべてを試してそれらを分類すれば良い
 *)

type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec add t x =
  match t with
      Lf -> Br(x, Lf, Lf)
    | Br(y, left, right) as whole when x = y -> whole
    | Br(y, left, right) ->
        if x < y
        then Br(y, add left x, right)
        else Br(y, left, add right x)

let empty = Lf

let rec fold_left f e l =
  match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest

let make_tree_by_add l = fold_left add empty l

(* 1234 *)
let tree1 = Br(1, Lf, Br(2, Lf, Br(3, Lf, Br(4, Lf, Lf))))
let check1 = tree1 = make_tree_by_add [1; 2; 3; 4]

(* 1243 *)
let tree2 = Br(1, Lf, Br(2, Lf, Br(4, Br(3, Lf, Lf), Lf)))
let check2 = tree2 = make_tree_by_add [1; 2; 4; 3]

(* 1324, 1342 *)
let tree3 = Br(1, Lf, Br(3, Br(2, Lf, Lf), Br(4, Lf, Lf)))
let check3 = tree3 = make_tree_by_add [1; 3; 2; 4]
let check4 = tree3 = make_tree_by_add [1; 3; 4; 2]

(* 1423 *)
let tree4 = Br(1, Lf, Br(4, Br(2, Lf, Br(3, Lf, Lf)), Lf))
let check5 = tree4 = make_tree_by_add [1; 4; 2; 3]

(* 1432 *)
let tree5 = Br(1, Lf, Br(4, Br(3, Br(2, Lf, Lf), Lf), Lf))
let check6 = tree5 = make_tree_by_add [1; 4; 3; 2]

(* 2134, 2314, 2341 *)
let tree6 = Br(2, Br(1, Lf, Lf), Br(3, Lf, Br(4, Lf, Lf)))
let check7 = tree6 = make_tree_by_add [2; 1; 3; 4]
let check8 = tree6 = make_tree_by_add [2; 3; 1; 4]
let check9 = tree6 = make_tree_by_add [2; 3; 4; 1]

(* 2143, 2413, 2431 *)
let tree7 = Br(2, Br(1, Lf, Lf), Br(4, Br(3, Lf, Lf), Lf))
let check10 = tree7 = make_tree_by_add [2; 1; 4; 3]
let check11 = tree7 = make_tree_by_add [2; 4; 1; 3]
let check12 = tree7 = make_tree_by_add [2; 4; 1; 3]

(* 3124, 3142, 3412 *)
let tree8 = Br(3, Br(1, Lf, Br(2, Lf, Lf)), Br(4, Lf, Lf))
let check13 = tree8 = make_tree_by_add [3; 1; 2; 4]
let check14 = tree8 = make_tree_by_add [3; 1; 4; 2]
let check15 = tree8 = make_tree_by_add [3; 4; 1; 2]

(* 3214, 3241, 3421 *)
let tree9 = Br(3, Br(2, Br(1, Lf, Lf), Lf), Br(4, Lf, Lf))
let check16 = tree9 = make_tree_by_add [3; 2; 1; 4]
let check17 = tree9 = make_tree_by_add [3; 2; 4; 1]
let check18 = tree9 = make_tree_by_add [3; 4; 2; 1]

(* 4123 *)
let tree10 = Br(4, Br(1, Lf, Br(2, Lf, Br(3, Lf, Lf))), Lf)
let check19 = tree10 = make_tree_by_add [4; 1; 2; 3]

(* 4132 *)
let tree11 = Br(4, Br(1, Lf, Br(3, Br(2, Lf, Lf), Lf)), Lf)
let check20 = tree11 = make_tree_by_add [4; 1; 3; 2]

(* 4213, 4231 *)
let tree12 = Br(4, Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf)), Lf)
let check21 = tree12 = make_tree_by_add [4; 2; 1; 3]
let check22 = tree12 = make_tree_by_add [4; 2; 3; 1]

(* 4312 *)
let tree13 = Br(4, Br(3, Br(1, Lf, Br(2, Lf, Lf)), Lf), Lf)
let check23 = tree13 = make_tree_by_add [4; 3; 1; 2]

(* 4321 *)
let tree14 = Br(4, Br(3, Br(2, Br(1, Lf, Lf), Lf), Lf), Lf)
let check24 = tree14 = make_tree_by_add [4; 3; 2; 1]