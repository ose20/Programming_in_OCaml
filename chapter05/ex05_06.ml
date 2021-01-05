(* 練習問題5.6 *)
let quick_sort l =
  let rec quick l result =
    match l with
      | [] -> result
      | [x] -> x :: result
      | pivot :: rest ->
          let rec partition left right result = function
            | [] -> quick left (pivot :: quick right result)
            | x :: rest -> 
                if x <= pivot
                then partition (x :: left) right result rest
                else partition left (x :: right) result rest
          in
          partition [] [] result rest
  in
  quick l []

let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor ( t/. m)

let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed::tail)

let check1 = quick_sort [4; 20; 2; 1; 0; 45; 33; 9]
let check2 = quick_sort (snd (randlist 100000 1.0 []))