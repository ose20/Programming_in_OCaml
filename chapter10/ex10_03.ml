(** 練習問題10.3 *)
(* -w オプションで行の長さを調整できる fold コマンドの実装 *)
(*  
  ASCII コード表にない文字は \ddd\ddd\ddd (ddd は 128 以上の数字)
  の 3 バイトが当てられてるっぽいので、そう仮定して
  String と Bytes のライブラリをよしなに使う
  また、それらの 3 バイト文字の横幅は ASCII 文字の 2 倍と仮定する
*)
let limit = ref 80
let filenames : string list ref = ref []
let special_width = 2

let spec = [
  ("-w", Arg.Int (fun i -> limit := i), 
  "Specify a line width to use instead of the default 80 columns")
]

let read_and_print fname =
  let width = ref 0 in
  let in_ch = open_in fname in
  let buffer = Bytes.create 3 in
  Printf.printf "========== %s ==========\n" fname;
  try
    while true do
      let c = input_char in_ch in
      begin
        if c = '\n'
        then (print_char '\n'; width := 0)
        else if int_of_char c <= 127
        then (print_char c; width := !width + 1)
        else
          (let c1 = input_char in_ch and c2 = input_char in_ch in
          Bytes.set buffer 0 c; Bytes.set buffer 1 c1; Bytes.set buffer 2 c2;
          print_string (Bytes.to_string buffer); width := !width + special_width)
      end;
      if !width >= !limit then (print_char '\n'; width := 0)
    done
  with End_of_file -> close_in in_ch


let solve fname =
  try read_and_print fname with
    Sys_error str -> Printf.printf "fold: %s\n" str

let _ =
  Arg.parse spec (fun s -> filenames := s :: !filenames)
  "Usage: fold [-w] filename ...";
  List.iter (fun fname -> solve fname) (List.rev !filenames)
