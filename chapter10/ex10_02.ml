(** 練習問題10.2 *)
(*  ファイル名の列を引数として、それぞれのファイルの行数(改行文字の数)、
    空白で区切られた単語の数、バイト数を表示する wc コマンドを実装する。
    オプションは、
    -c バイト数だけ表示
    -l 行数だけ表示
    -w 単語数だけ表示
    を実装する。注意点は、オプションが何も指定されなかった場合はすべてを
    表示するが、何かが指定された場合はその指定されたものだけを表示する
    ようにすること。
*)

type option_char = C | L | W

let display_byte = ref false
let display_line = ref false
let display_word = ref false
let filenames : string list ref = ref []

let spec = [
  ("-c", Arg.Set display_byte, "Display the number of bytes of each input file");
  ("-l", Arg.Set display_line, "Display the number of lines of each input file");
  ("-w", Arg.Set display_word, "Display the number of words of each input file")
]

let _ =
  Arg.parse spec (fun s -> filenames := s :: !filenames)
  "Usage: wc [-c] [-l] [-w] [-help] filename ..."

(* 何も指定されなかった場合にすべてのフラグをオンにする *)
let _ =
  if !display_byte = false && !display_line = false && !display_word = false
  then (display_byte := true; display_line := true; display_word := true)
  else ()

let input_until_newline in_ch =
  let rec iter () =
    if input_char in_ch = '\n' then () else iter ()
  in
  iter ()

let input_until_space in_ch =
  (* まず次の単語の先頭の文字を読み込むまで進める *)
  (* ファイルが終わったときの例外はここで投げてもらう *)
  while let c = input_char in_ch in c = ' ' || c = '\n' || c = '\t' do () done;
  (* ここまでこれたら、少なくとも今回の呼び出しにおいては、空白の後に *)
  (* ファイルの終わりが来ていることはない *)
  (* 次は ' ' か '\n' '\t' が来るまで文字の読み込みを続けるが、途中でファイルが終わっても *)
  (* 例外を投げないようにしたい *)
  (* 改行文字も単語の区切りとみなす *)
  let rec iter () =
    let c = input_char in_ch in
    if c = ' ' || c = '\n' || c = '\t' then () else iter ()
  in
  try iter () with End_of_file -> ()


let num_of_each fname opt_char =
  let in_ch = open_in fname in
  let cnt = ref 0 in
  let rec iter () =
    match opt_char with
        C -> (try ignore (input_byte in_ch); incr cnt; iter () with End_of_file -> ())
      | L -> (try input_until_newline in_ch; incr cnt; iter () with End_of_file -> ())
      | W -> (try input_until_space in_ch; incr cnt; iter () with End_of_file -> ())
  in
  (try iter () with End_of_file -> close_in in_ch); !cnt
  

let print_contents fname =
  begin
    if !display_line
    then Printf.printf "%8d " @@ num_of_each fname L
  end;
  begin
    if !display_word
    then Printf.printf "%8d " @@ num_of_each fname W
  end;
  begin
    if !display_byte
    then Printf.printf "%8d " @@ num_of_each fname C
  end;
  Printf.printf "%s\n" fname



let _ = 
  if !display_line then Printf.printf "%8s " "lines" else ();
  if !display_word then Printf.printf "%8s " "words" else ();
  if !display_byte then Printf.printf "%8s " "bytes" else ();
  Printf.printf "\n";
  List.iter
  (fun fname -> try print_contents fname with Sys_error str -> Printf.printf "wc: %s\n" str)
  (List.rev !filenames)
              
