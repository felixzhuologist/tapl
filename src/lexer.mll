{
open Lexing
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+

let ident = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*

let intv = ['0'-'9']+

rule read =
  parse
  | white    { read lexbuf }
  | "lambda" { LAMBDA }
  | "Bool"   { BOOL }
  | "Nat"    { NAT }
  | "λ"      { LAMBDA }
  | "->"     { ARROW }
  | "."      { DOT }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | ":"      { COLON }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "succ"   { SUCC }
  | "pred"   { PRED }
  | "iszero" { ISZERO }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | ident    { IDENT (Lexing.lexeme lexbuf) }
  | intv     { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
