%{
open Syntax
%}

%token <int> VAR
%token LAMBDA

%token DOT
%token LPAREN
%token RPAREN
%token EOF

%token TRUE
%token FALSE
%token ZERO

%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO

%start toplevel
%type <Syntax.term> toplevel

%%
toplevel:
  /* todo: use option */
  | EOF                 { TmVar(0, 0) }
  | term EOF            { $1 } ;

term:
  | AppTerm             { $1 }
  | LAMBDA DOT term     { TmAbs($3) }
  | IF term THEN term ELSE term { TmIf($2, $4, $6) } ;

AppTerm:
  | ATerm               { $1 }
  | AppTerm ATerm       { TmApp($1, $2) }
  | SUCC ATerm          { TmSucc($2) }
  | PRED ATerm          { TmPred($2) }
  | ISZERO ATerm        { TmIsZero($2) } ;

ATerm:
  | LPAREN term RPAREN  { $2 }
  | VAR                 { TmVar(0, $1) }
  | ZERO                { TmZero }
  | TRUE                { TmTrue }
  | FALSE               { TmFalse } ;