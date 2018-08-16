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
%token <int> INTV

%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO

%token TYBOOL
%token TYNAT
%token ARROW
%token COLON

%token UNIT
%token TYUNIT
%token SEMICOLON
%token AS

%token LET
%token EQ
%token IN

%start toplevel
%type <Syntax.context -> Syntax.term> toplevel

%%
toplevel:
  /* todo: use option */
  | EOF                 { fun _ -> TmFalse }
  | term EOF            { fun ctx -> $1 ctx } ;

term:
  | AppTerm
    { $1 }
  | LAMBDA IDENT COLON Type DOT term 
    { fun ctx ->
        let ctx1 = addbinding ctx $2 NameBind in
        TmAbs($2, $4, $6 ctx1) }
  | IF term THEN term ELSE term
    { fun ctx -> TmIf($2 ctx, $4 ctx, $6 ctx) }
  (* NOTE: using derived forms currently means that the derived form will 
   * show up in cases where a term doesn't get evaluated (e.g. the body of a
   * function: lambda x: Nat . (x as Nat) will eval to λx.λid.x x : Nat -> Nat) *)
  | term SEMICOLON term
    { fun ctx ->
        let ctx1 = addbinding ctx "_" NameBind in
        TmApp(TmAbs("_", TyUnit, $3 ctx1), $1 ctx) }
  | term AS Type
    { fun ctx ->
        let ctx1 = addbinding ctx "id" NameBind in
        TmApp(TmAbs("id", $3, $1 ctx1), $1 ctx) }
  | LET IDENT EQ term IN term { fun ctx -> TmLet($2, $4 ctx, $6 ctx) };

AppTerm:
  | ATerm               { $1 }
  | AppTerm ATerm       { fun ctx -> TmApp($1 ctx, $2 ctx) }
  | SUCC ATerm          { fun ctx -> TmSucc($2 ctx) }
  | PRED ATerm          { fun ctx -> TmPred($2 ctx) }
  | ISZERO ATerm        { fun ctx -> TmIsZero($2 ctx) } ;

ATerm:
  | LPAREN term RPAREN    { $2 }
  | IDENT                 { fun ctx -> TmVar(name2index ctx $1, ctxlength ctx) }
  | INTV
    { let rec f n = match n with
          | 0 -> TmZero
          | n -> TmSucc(f (n - 1))
        in fun _ -> f $1 }
  | TRUE                  { fun _ -> TmTrue }
  | FALSE                 { fun _ -> TmFalse }
  | UNIT                  { fun _ -> TmUnit } ;

Type:
  | AType            { $1 }
  | AType ARROW Type { TyArr($1, $3) } ;

AType:
  | LPAREN Type RPAREN   { $2 }
  | TYUNIT               { TyUnit }
  | TYBOOL               { TyBool }
  | TYNAT                { TyNat } ;
