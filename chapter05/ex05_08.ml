(* 練習問題5.8 *)
let reverse lst =
  let rec iter result = function
    | [] -> result
    | x :: rest -> iter (x :: result) rest
  in iter [] lst

let map2 f lst =
  let rec tailmap f result lst =
    match lst with
      | [] -> result
      | x :: rest -> tailmap f (f x :: result) rest
  in reverse @@ tailmap f [] lst

let li1 = [1; 2; 3; 4; 5]
let check1 = map2 succ li1