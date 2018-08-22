open Syntax
open Context

let rec printty ty = match ty with
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyUnit -> "Unit"
  | TyRecord(tys) ->
      let printfield (label, fieldty) = label ^ "=" ^ (printty fieldty) in
      "{" ^ (String.concat ", " (List.map printfield tys)) ^ "}"
  | TyArr(ty1, ty2) -> printty ty1 ^ " -> " ^ printty ty2
  | TyVariant(tys) ->
      let printfield (label, fieldty) = label ^ ": " ^ (printty fieldty) in
      "<" ^ (String.concat ", " (List.map printfield tys)) ^ ">" 
  | TyRef(ty) -> "Ref " ^ printty ty

let rec printtm (ctx: context) (t: term) = match t with
  | TmVar(i, n) ->
      if ctxlength ctx = n then
        let (n, _) = getbinding ctx i in n
      else
        "bad index"
  | TmAbs(x, _, t) ->
      let ctx', x' = pickfreshname ctx x in
      ("λ" ^ x' ^ "." ^ printtm ctx' t)
  | TmApp(t1, t2) -> printtm ctx t1 ^ " " ^ printtm ctx t2
  | TmIf(t1, t2, t3) -> 
      "if " ^ printtm ctx t1 ^
      " then " ^ printtm ctx t2 ^
      " else " ^ printtm ctx t3
  | TmSucc(t) ->
      let rec f n t = match t with
        | TmZero -> string_of_int n
        | TmSucc(s) -> f (n + 1) s
        | _ -> "succ " ^ printtm ctx t
      in f 1 t 
  | TmPred(t) -> "pred " ^ printtm ctx t
  | TmIsZero(t) -> "iszero " ^ printtm ctx t
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"
  | TmUnit -> "unit"
  | TmLet(n, t1, t2) ->
      "let " ^ n ^ "=" ^
      printtm ctx t1 ^ " in " ^
      printtm (addbinding ctx n NameBind) t2
  | TmRecord(fields) ->
      let printfield (label, field) = label ^ "=" ^ printtm ctx field in
      let fs = List.map printfield fields in
      "{" ^ (String.concat "," fs) ^ "}"
  | TmProj(t, l) -> printtm ctx t ^ "." ^ l
  | TmAscribe(t, ty) -> printtm ctx t ^ " as " ^ printty ty
  | TmTag(s, t) -> "<" ^ s ^ "=" ^ printtm ctx t ^ ">"
  | TmFix(t) -> "fix " ^ printtm ctx t
  | TmRef(t) -> "ref " ^ printtm ctx t
  | TmLoc(i) -> "<loc #" ^ string_of_int i ^ ">"
  | _ -> "TODO"
