(** 練習問題10.1 *)
(* 最低限 -n オプションを持った cat コマンドの実装 *)
(* 存在しないものはその旨を出力するが、それでプログラム自体は終了しないようにする *)
(* -cry で猫が鳴く *)
let display_linenum = ref false
let filenames : string list ref = ref []

let spec = [
  ("-n", Arg.Set display_linenum, "Display line number");
  ("-cry", Arg.Unit (fun () -> print_endline "にゃ〜ん"), "A cat cries")
]

let _ =
  Arg.parse spec (fun s -> filenames := s :: !filenames)
  "Usage: cat [-cry] [-n] [-help] filename"

let print_content fname =
  let in_ch = open_in fname in
  let () = Printf.printf "========== %s ==========\n" fname in
  let rec iter i =
    let line = input_line in_ch in
    begin
      if !display_linenum
      then Printf.printf "%3d: %s\n" i line
      else Printf.printf "%s\n" line
    end; iter (i + 1)
  in
  try iter 1 with
    End_of_file -> close_in in_ch

let _ =
  List.iter 
  (fun s -> try print_content s with Sys_error str -> Printf.printf "cat: %s\n" str) 
  (List.rev !filenames)