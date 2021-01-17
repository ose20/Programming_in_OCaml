(** 練習問題8.7 *)
let array_iter f ary =
  let n = Array.length ary in
  for i = 0 to n - 1 do
    f ary.(i)
  done

let _ = array_iter (fun s -> print_string "Station: "; print_endline s)
  [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|]

let _ = array_iter (fun s -> Printf.printf "%s\n" s) 
  [|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|]