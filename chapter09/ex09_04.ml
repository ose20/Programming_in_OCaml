(** 練習問題9.4 *)
(*  抽象データ型を表す次の二つのシグネチャを持つモジュールは  
    どちらもあまり実用上意味がない。それはなぜか            *)

module type BOGUS1 = 
  sig
    type t
    val f: t -> int -> t
  end

module type BOGUS2 =
  sig
    type t
    val e: t
  end

(*  まず BOGUS1 については、t 型を持つ値を作れないので、関数 f も使えず
    結果として何もできない。
    次に BOGUS2 については、t 型を持つ値は作れるものの、それは e のみで、
    それを利用する関数も使えないので、ただ1つの値 e を作ることしかできない  *)

