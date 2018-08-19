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
  | "unit"   { UNIT }
  | "lambda" { LAMBDA }
  | "Bool"   { TYBOOL }
  | "Nat"    { TYNAT }
  | "Unit"   { TYUNIT }
  | "λ"      { LAMBDA }
  | "->"     { ARROW }
  | "=>"     { FATARROW }
  | "."      { DOT }
  | ","      { COMMA }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "{"      { LCURLY }
  | "}"      { RCURLY }
  | "<"      { LT }
  | ">"      { GT }
  | ":"      { COLON }
  | ";"      { SEMICOLON }
  | "="      { EQ }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "succ"   { SUCC }
  | "pred"   { PRED }
  | "iszero" { ISZERO }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "as"     { AS }
  | "let"    { LET }
  | "letrec" { LETREC }
  | "in"     { IN }
  | "case"   { CASE }
  | "of"     { OF }
  | "fix"    { FIX }
  | ident    { IDENT (Lexing.lexeme lexbuf) }
  | intv     { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
