(** 練習問題9.2 *)
(* 二分探索木を使ったテーブルを、シグネチャとして TABLE2 を与えたモジュールとして実装する *)

(* シグネチャ TABLE2 *)
module type TABLE2 =
  sig
    type ('a, 'b) t
    val empty : ('a, 'b) t
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val retrieve : 'a -> ('a, 'b) t -> 'b option
    val dump : ('a, 'b) t -> ('a * 'b) list
  end

module Table : TABLE2 =
  struct
    type ('a, 'b) t =
        Lf
      | Br of 'a * 'b * ('a, 'b) t * ('a, 'b) t
    
    let empty = Lf

    let rec add key data = function
        Lf -> Br(key, data, Lf, Lf)
      | Br(key', data', left, right) ->
          if key = key'
          then Br(key', data, left, right)
          else if key < key'
          then Br(key', data', add key data left, right)
          else Br(key', data', left, add key data right)
    
    let rec retrieve key = function
        Lf -> None
      | Br(key', data, left, right) ->
          if key = key' then Some data
          else if key < key' then retrieve key left
          else retrieve key right

    (* 各要素に f を施しながら inorder で木を巡回してリストを作る *)
    let inord_map f t =
      let rec inord_iter t rest =
        match t with
            Lf -> rest
          | Br(key, data, left, right) ->
              inord_iter left ((f key data) :: inord_iter right rest)
      in
      inord_iter t []
    
    let dump t = inord_map (fun key data -> (key, data)) t
  end

let ta = 
  Table.add 5 5
  @@ Table.add 7 7
  @@ Table.add 8 8
  @@ Table.add 5 5
  @@ Table.add 1 1
  @@ Table.add 3 3 
  @@ Table.add 6 6 
  Table.empty
