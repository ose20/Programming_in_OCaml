(** 練習問題12.3 *)
(* 1 + 2 + 3 のような連続した演算を行えるような電卓クラスを作れ *)
(*
  さらに
  1. 減算と乗算も扱う
  2. 電卓の C ボタンのようにリセットをできるようにする
  3. 演算記号の上書きができるようにする
  4. 演算は左結合的とする
 *)
class calc =
  object
    val mutable result = 0        (* 計算結果を格納しておく *)
    val mutable num = 0           (* ユーザーからの入力を受け取る *)
    val mutable func = fun x -> x

    method clear =
      result <- 0; num <- 0; func <- (fun x -> x)
    method input n = num <- n; result <- func num
    method plus = func <- (fun y -> result + y)
    method minus = func <- (fun y -> result - y)
    method mul = func <- (fun y -> result * y)
    method eq = result
  end

let c = new calc
(* 1 plus 4 plus 7 eq -> 12 *)
let test1 = c#input 1; c#plus; c#input 4; c#plus; c#input 7; c#eq = 12

(* 演算記号の直後に = を押した場合 *)
(* 1 plus 4 plus 7 plus eq -> 12 *)
let _ = c#clear
let test2 = c#input 1; c#plus; c#input 4; c#plus;
  c#input 7; c#plus; c#eq = 12

(* 演算記号ごちゃ混ぜ *)
(* 1 plus 4 minus 7 mul 10 eq -> -20 *)
let _ = c#clear
let test3 = c#input 1; c#plus; c#input 4; c#minus;
  c#input 7; c#mul; c#input 10; c#eq = -20

(* 演算記号を上書きするケース *)
(* 5 plus minus 3 plus mul 4 eq -> 8 *)
let _ = c#clear
let test4 = c#input 5; c#plus; c#minus; c#input 3;
  c#plus; c#mul; c#input 4; c#eq = 8