open Format
open Lexer
open Lexing
open Syntax

let line_stream_of_channel channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

let process_line line =
  try
    let lexbuf = Lexing.from_string line in
    let ast = (Parser.toplevel Lexer.read lexbuf) emptycontext in
    let result = eval emptycontext ast in
    let ty = typeof emptycontext ast in
    print_endline ((printtm emptycontext result) ^ " : " ^ (printty ty))
  with
    | SyntaxError msg -> prerr_endline msg
    | Parser.Error -> prerr_endline "Parsing error"
    | Syntax.TypeError -> prerr_endline "Type error"

let _ =
  if (Array.length Sys.argv) > 1 then
    let f = open_in Sys.argv.(1) in
    try
      Stream.iter
        (fun line -> print_endline line; process_line line)
        (line_stream_of_channel f);
      close_in f
    with e ->
      close_in f;
      raise e
  else
    while true do
      output_string stdout "> ";
      flush stdout;
      let line = input_line stdin in
      process_line line
    done
