{
open Lexing
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+

let int = ['0'-'9']+

rule read =
  parse
  | int      { VAR (int_of_string (Lexing.lexeme lexbuf)) }
  | white    { read lexbuf }
  | "lambda" { LAMBDA }
  | "λ"      { LAMBDA }
  | "."      { DOT }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "succ"   { SUCC }
  | "pred"   { PRED }
  | "iszero" { ISZERO }
  | "0"      { ZERO }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
