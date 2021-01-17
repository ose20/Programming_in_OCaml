(** 練習問題8.8 *)
let array_iteri f ary =
  let n = Array.length ary in
  for i = 0 to n - 1 do
    f (i + 1) ary.(i)
  done

let array_iteri2 f ary =
  let rec iteri i =
    try f (i + 1) ary.(i); iteri (i + 1) with
      Invalid_argument s -> ()
  in
  iteri 0

let f i s = Printf.printf "Station #%d: %s\n" i s

let _ = array_iteri f [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|]
let _ = array_iteri2 f [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|]
