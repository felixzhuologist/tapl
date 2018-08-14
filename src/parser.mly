%{
open Syntax
%}

%token <string> IDENT
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
%type <Syntax.context -> Syntax.term> toplevel

%%
toplevel:
  /* todo: use option */
  | EOF                 { fun _ -> TmVar(0, 0) }
  | term EOF            { fun ctx -> $1 ctx } ;

term:
  | AppTerm
    { $1 }
  | LAMBDA IDENT DOT term 
    { fun ctx ->
        let ctx1 = addname ctx $2 NameBind in
        TmAbs($2, $4 ctx1) }
  | IF term THEN term ELSE term
    { fun ctx -> TmIf($2 ctx, $4 ctx, $6 ctx) } ;

AppTerm:
  | ATerm               { $1 }
  | AppTerm ATerm       { fun ctx -> TmApp($1 ctx, $2 ctx) }
  | SUCC ATerm          { fun ctx -> TmSucc($2 ctx) }
  | PRED ATerm          { fun ctx -> TmPred($2 ctx) }
  | ISZERO ATerm        { fun ctx -> TmIsZero($2 ctx) } ;

ATerm:
  | LPAREN term RPAREN    { $2 }
  | IDENT                 { fun ctx -> TmVar(name2index ctx $1, ctxlength ctx) }
  | ZERO                  { fun _ -> TmZero }
  | TRUE                  { fun _ -> TmTrue }
  | FALSE                 { fun _ -> TmFalse } ;