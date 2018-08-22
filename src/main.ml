open Context
open Format
open Lexer
open Lexing
open Print
open Store

let line_stream_of_channel channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

let paragraphs lines =
    let rec next para_lines i =
      match Stream.peek lines, para_lines with
      | None, [] -> None
      | Some "", [] ->
          Stream.junk lines;
          next para_lines i
      | Some "", _ | None, _ ->
          Some (String.concat "\n" (List.rev para_lines))
      | Some line, _ ->
          Stream.junk lines;
          next (line :: para_lines) i in
    Stream.from (next []);;

let process_term s =
  try
    let lexbuf = Lexing.from_string s in
    let ast = (Parser.toplevel Lexer.read lexbuf) emptycontext in
    let (result, _) = Eval.eval emptycontext emptystore ast in
    let ty = Types.typeof emptycontext ast in
    print_endline ((printtm emptycontext result) ^ " : " ^ (printty ty))
  with
    | SyntaxError msg -> prerr_endline msg
    | Parser.Error -> prerr_endline "Parsing error"
    | Types.TypeError -> prerr_endline "Type error"

let _ =
  if (Array.length Sys.argv) > 1 then
    let f = open_in Sys.argv.(1) in
    try
      Stream.iter
        (fun para -> process_term para)
        (paragraphs (line_stream_of_channel f));
      close_in f
    with e ->
      close_in f;
      raise e
  else
    while true do
      output_string stdout "> ";
      flush stdout;
      let line = input_line stdin in
      process_term line
    done
