{
open Lexing
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+

rule read =
  parse
  | white    { read lexbuf }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "succ"   { SUCC }
  | "pred"   { PRED }
  | "iszero" { ISZERO }
  | "0"      { ZERO }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
