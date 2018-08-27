%{
open Syntax
open Context
%}

%token <string> IDENT
%token LAMBDA

%token DOT
%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LT
%token GT
%token COMMA
%token EOF
%token VBAR

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
%token FATARROW
%token COLON

%token UNIT
%token TYUNIT
%token SEMICOLON
%token AS
%token LET
%token LETREC
%token EQ
%token IN
%token CASE
%token OF
%token FIX

%token REF
%token BANG
%token ASSIGN
%token TYREF
%token TYTOP

%token MU
%token <string> TYIDENT

%start toplevel
%type <Context.context -> Syntax.term> toplevel

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
        TmAbs($2, $4 ctx1, $6 ctx1) }
  | IF term THEN term ELSE term
    { fun ctx -> TmIf($2 ctx, $4 ctx, $6 ctx) }
  | LET IDENT EQ term IN term
    { fun ctx ->
        let ctx1 = addbinding ctx $2 NameBind in
        TmLet($2, $4 ctx, $6 ctx1) }
  | LETREC IDENT COLON Type EQ term IN term
    { fun ctx ->
        let ctx1 = addbinding ctx $2 NameBind in
        let recfunc = TmFix(TmAbs($2, $4 ctx1, $6 ctx1)) in
        TmLet($2, recfunc, $8 ctx1) }
  | CASE term OF Cases
        { fun ctx -> TmCase($2 ctx, $4 ctx) }
  | AppTerm ASSIGN AppTerm
    { fun ctx -> TmAssign($1 ctx, $3 ctx) } ;

AppTerm:
  | PathTerm                { $1 }
  | AppTerm PathTerm        { fun ctx -> TmApp($1 ctx, $2 ctx) }
  | SUCC PathTerm           { fun ctx -> TmSucc($2 ctx) }
  | PRED PathTerm           { fun ctx -> TmPred($2 ctx) }
  | ISZERO PathTerm         { fun ctx -> TmIsZero($2 ctx) }
  | FIX PathTerm            { fun ctx -> TmFix($2 ctx) }
  | REF PathTerm            { fun ctx -> TmRef($2 ctx) }
  | BANG PathTerm           { fun ctx -> TmDeref($2 ctx) } ;
  | LT IDENT EQ PathTerm GT { fun ctx -> TmTag($2, $4 ctx) }

PathTerm:
  | PathTerm DOT INTV  { fun ctx -> TmProj($1 ctx, string_of_int $3)}
  | PathTerm DOT IDENT { fun ctx -> TmProj($1 ctx, $3)}
  | AscribeTerm        { $1 } ;

AscribeTerm:
  | ATerm AS Type               { fun ctx -> TmAscribe($1 ctx, $3 ctx) }
  | ATerm                       { $1 } ;

TermSeq:
  | term { $1 }
  (* NOTE: using derived forms currently means that the derived form will 
   * show up in cases where a term doesn't get evaluated (e.g. the body of a
   * function: lambda x: Nat . (x as Nat) will eval to λx.λid.x x : Nat -> Nat) *)
  | term SEMICOLON TermSeq
    { fun ctx ->
        let ctx1 = addbinding ctx "_" NameBind in
        TmApp(TmAbs("_", TyUnit, $3 ctx1), $1 ctx) } ;

ATerm:
  | LPAREN TermSeq RPAREN { $2 }
  | LCURLY Fields RCURLY  { fun ctx -> TmRecord($2 ctx 1) }
  | IDENT                 { fun ctx -> TmVar(name2index ctx $1, ctxlength ctx) }
  | INTV
    { let rec f n = match n with
          | 0 -> TmZero
          | n -> TmSucc(f (n - 1))
        in fun _ -> f $1 }
  | TRUE                  { fun _ -> TmTrue }
  | FALSE                 { fun _ -> TmFalse }
  | UNIT                  { fun _ -> TmUnit } ;

Cases:
  | Case            { fun ctx -> [$1 ctx]}
  | Case VBAR Cases { fun ctx -> ($1 ctx)::($3 ctx) } ;

Case:
  (* TODO: replace ATerm with term without running into S/R conflicts *)
  | LT IDENT EQ IDENT GT FATARROW ATerm 
      { fun ctx ->
          let ctx1 = addbinding ctx $4 NameBind in
          ($2, ($4, $7 ctx1)) } ;

Fields:
  | /* empty */
    { fun _ _ -> [] }
  | NEFields
    { $1 } ;

(* Take in an index in addition to the context to support implicit labels -
 * if no label is provided, then we use the index as the label. This also
 * means that tuples are implemented as records under the hood *)
NEFields:
  | Field                { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields 
      (* overwrite field values for labels that already exist, which makes things like
       * {a=1, a=2} evaluate to {a=2} *)
      { fun ctx i ->
          let (new_label, f) = ($1 ctx i) in
          let existing_fields = ($3 ctx (i+1)) in
          if List.mem_assoc new_label existing_fields then
          existing_fields else
          (new_label, f) :: existing_fields } ;

Field:
  | IDENT EQ term { fun ctx _ -> ($1, $3 ctx) }
  | term          { fun ctx i -> (string_of_int i, $1 ctx) } ;

(* type fields separated by = *)
TypeFieldsEq:
  | /* empty */
    { fun _ _ -> [] }
  | NETypeFieldsEq { $1 } ;

NETypeFieldsEq:
  | TypeFieldEq { fun ctx i -> [$1 ctx i] } 
  | TypeFieldEq COMMA NETypeFieldsEq
      { fun ctx i ->
          let (new_label, ty) = ($1 ctx i) in
          let existing_fields = ($3 ctx (i+1)) in
          if List.mem_assoc new_label existing_fields then
          existing_fields else
          (new_label, ty) :: existing_fields } ;

TypeFieldEq:
  | IDENT EQ Type { fun ctx _ -> ($1, $3 ctx) }
  | Type          { fun ctx i -> (string_of_int i, $1 ctx) } ;

(* type fields separated by : *)
TypeFieldsColon:
  | /* empty */
    { fun _ -> [] }
  | NETypeFieldsColon { fun ctx -> $1 ctx } ;

NETypeFieldsColon:
  | TypeFieldColon { fun ctx -> [$1 ctx] } 
  | TypeFieldColon COMMA NETypeFieldsColon
      { fun ctx ->
          let (new_label, ty) = $1 ctx in
          let existing_fields = $3 ctx in
          if List.mem_assoc new_label existing_fields then
          existing_fields else
          (new_label, ty) :: existing_fields } ;  

TypeFieldColon:
  | IDENT COLON Type { fun ctx -> ($1, $3 ctx) } ;

Type:
  | AType            { fun ctx -> $1 ctx }
  | TYREF AType      { fun ctx -> TyRef($2 ctx) }
  | AType ARROW Type { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | MU TYIDENT DOT Type
      { fun ctx ->
          let ctx1 = addbinding ctx $2 NameBind in
          TyRec($2, $4 ctx1) } ;

AType:
  | LPAREN Type RPAREN         { fun ctx -> $2 ctx }
  | LCURLY TypeFieldsEq RCURLY { fun ctx -> TyRecord($2 ctx 1) }
  | LT TypeFieldsColon GT      { fun ctx -> TyVariant($2 ctx) }
  | TYIDENT                    { fun ctx -> TyVar(name2index ctx $1, ctxlength ctx)}
  | TYUNIT                     { fun _ -> TyUnit }
  | TYBOOL                     { fun _ -> TyBool }
  | TYNAT                      { fun _ -> TyNat }
  | TYTOP                      { fun _ -> TyTop } ;
