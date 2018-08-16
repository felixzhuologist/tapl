open Format
open Lexer
open Lexing
open Syntax

let _ =
  while true do
    output_string stdout "> ";
    flush stdout;
    try
      let input_str = input_line stdin in
      let lexbuf = Lexing.from_string input_str in
      let ast = (Parser.toplevel Lexer.read lexbuf) emptycontext in
      let result = eval ast in
      let ty = typeof emptycontext result in
      print_endline ((printtm emptycontext result) ^ " : " ^ (printty ty))
    with
      | SyntaxError msg -> prerr_endline msg
      | Parser.Error -> prerr_endline "Parsing error"
  done
