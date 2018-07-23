{
open Lexing
open Parser

exception SyntaxError of string
}

rule read =
  parse
  | "0"      { ZERO }
  | "true"   { TRUE }
  | "false" { FALSE }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
