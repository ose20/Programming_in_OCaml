(* 練習問題8.3 *)
let f = ref (fun y -> y + 3943431)

let funny_fact x =
  if x = 1 then 1
  else x * (!f (x - 1))

let _ = f := funny_fact

let check1 = funny_fact 2
let check2 = funny_fact 5

(*  
    通常再帰的に関数を呼ぶところを、型が同じ関数への参照と差し替える
    その後、参照先を今定義した funny_fact にすることで通常の再帰関数と
    等価な関数ができる    
*)