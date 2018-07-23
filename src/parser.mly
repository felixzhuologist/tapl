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

%token LPAREN
%token RPAREN

%start term
%type <Syntax.term> term

%%
term:
  | appterm                      { $1 }
  | IF term THEN term ELSE term  { TmIf($2, $4, $6) } ;

appterm:
  | aterm        { $1 }
  | SUCC aterm   { TmSucc($2) }
  | PRED aterm   { TmPred($2) }
  | ISZERO aterm { TmIsZero($2) } ;

aterm:
  | LPAREN term RPAREN { $2 }
  | ZERO               { TmZero }
  | TRUE               { TmTrue }
  | FALSE              { TmFalse } ;
