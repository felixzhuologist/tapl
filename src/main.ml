open Core

let _ =
  while true do
    Out_channel.output_string stdout "> ";
    Out_channel.flush stdout;
    let input_str = In_channel.input_line In_channel.stdin in
    fprintf stdout "TODO\n"
  done
