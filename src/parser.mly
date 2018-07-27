%{
open Syntax
%}

%token <int> VAR
%token LAMBDA

%token DOT
%token LPAREN
%token RPAREN
%token EOF

%start toplevel
%type <Syntax.term> toplevel

%%
toplevel:
  /* todo: use option */
  | EOF                 { TmVar(0, 0) }
  | term EOF            { $1 } ;

term:
  | AppTerm             { $1 }
  | LAMBDA DOT term     { TmAbs($3) } ;

AppTerm:
  | ATerm               { $1 }
  | AppTerm ATerm       { TmApp($1, $2) } ;

ATerm:
  | LPAREN term RPAREN  { $2 }
  | VAR                 { TmVar(0, $1) } ;