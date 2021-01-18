(** 練習問題8.11 *)
(** ファイル名を引数にとって、そのファイルの内容を行番号付きで表示させる関数 *)
let display_file fname =
  let input_chan = open_in fname in
  let rec iter i =
    try Printf.printf "%3d: %s\n" i @@ input_line input_chan; iter (i + 1) with
      End_of_file ->  ()
  in
  iter 1; close_in input_chan

let display_file2 fname =
  let input_chan = open_in fname in
  let rec iter i =
    Printf.printf "%3d: %s\n" i @@ input_line input_chan;
    iter (i + 1)
  in
  try iter 1 with
    End_of_file -> close_in input_chan
  
let display_file3 fname =
  let input_chan = open_in fname in
  let rec iter i =
    let print_line () =
      let str = input_line input_chan in
      Printf.printf "%3d: %s\n" i str
    in
    try print_line (); iter (i + 1) with
      End_of_file -> ()
  in
  iter 1; close_in input_chan