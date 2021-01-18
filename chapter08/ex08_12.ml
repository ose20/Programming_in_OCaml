(** 練習問題8.12 *)
let cp from_name to_name =
  let in_chan = open_in from_name in
  let out_chan = open_out to_name in
  let rec iter () =
    let str = input_line in_chan in
    output_string out_chan str;
    output_string out_chan "\n";
    iter ()
  in
  try iter () with
    End_of_file ->
      begin
        flush out_chan;
        close_in in_chan;
        close_out out_chan
      end