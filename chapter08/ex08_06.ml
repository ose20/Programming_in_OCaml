(* 練習問題8.6 *)
(* body はループ変数を受け取るようにする *)
let for_exp from until body =
  let rec iter i =
    if i > until
    then ()
    else 
      begin
        body i;
        iter (i + 1)
      end
  in
  iter from

let shout () = for_exp 1 10 (fun _ -> print_endline "おはようございます！")

let res = ref 0
let _ = for_exp 5 10 (fun i -> res := !res + i)
let sum = !res