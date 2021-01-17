(* 練習問題8.4 *)

(* while を使う *)
let fib1 n =
  let i = ref 1 in
  let x = ref 1 and y = ref 0 in
  while !i < n do
    i := !i + 1;
    let z = !x in
    x := !x + !y;
    y := z
  done;
  !x

let lst = List.map fib1 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]

(* for を使う *)
let fib2 n =
  let x = ref 1 and y = ref 0 in
  for i = 2 to n do
    let z = !x in
    x := !x + !y;
    y := z
  done;
  !x

let lst = List.map fib2 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]