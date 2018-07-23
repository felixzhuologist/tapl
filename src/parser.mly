%{
open Syntax
%}

%token TRUE
%token FALSE
%token ZERO

%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO

%start main
%type <Syntax.term> main

%%
main:
  | s = ZERO  { TmZero }
  | t = TRUE  { TmTrue }
  | f = FALSE { TmFalse } ;
