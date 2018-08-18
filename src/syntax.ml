type ty =
  | TyBool
  | TyNat
  | TyUnit
  | TyTuple of ty list
  | TyArr of ty * ty

type term = 
  | TmVar of int * int
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmZero
  | TmIf of term * term * term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmUnit
  | TmLet of string * term * term
  | TmTuple of term list
  | TmProj of term * int

type binding =
  | NameBind
  | VarBind of ty

exception LookupFailure

type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding ctx name bind = (name, bind) :: ctx

let rec isnamebound ctx name =
  match ctx with
  | [] -> false 
  | (x,_)::rest -> if x=name then true else isnamebound rest name

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let getbinding ctx x =
  try
    List.nth ctx x
  with Failure _ ->
    raise LookupFailure

let rec name2index (ctx: context) (x: string) =
  match ctx with
    | [] -> raise LookupFailure
    | (y,_)::rest -> if y=x then 0 else 1 + (name2index rest x)

let rec isnumericval t = match t with
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1 
  | _ -> false

let termShift d t =
  let rec walk c t = match t with
    | TmTrue -> TmTrue
    | TmFalse -> TmFalse
    | TmZero -> TmZero
    | TmUnit -> TmUnit
    | TmIf(t1, t2, t3) -> TmIf(walk c t1, walk c t2, walk c t3)
    | TmSucc(t1) -> TmSucc(walk c t1)
    | TmPred(t1) -> TmPred(walk c t1)
    | TmIsZero(t1) -> TmIsZero(walk c t1)
    | TmVar(x, n) -> if x >= c then TmVar(x + d, n + d) else TmVar(x, n + d)
    | TmAbs(x, ty, t) -> TmAbs(x, ty, walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    | TmLet(n, t1, t2) -> TmLet(n, walk c t1, walk (c+1) t2) 
    | TmTuple(fields) -> TmTuple(List.map (walk c) fields)
    | TmProj(t, i) -> TmProj(walk c t, i)
  in walk 0 t

(* [ j -> s ]t *)
let termSubst j s t =
  let rec walk c t = match t with
    | TmTrue -> TmTrue
    | TmFalse -> TmFalse
    | TmZero -> TmZero
    | TmUnit -> TmUnit
    | TmIf(t1, t2, t3) -> TmIf(walk c t1, walk c t2, walk c t3)
    | TmSucc(t1) -> TmSucc(walk c t1)
    | TmPred(t1) -> TmPred(walk c t1)
    | TmIsZero(t1) -> TmIsZero(walk c t1)
    | TmVar(x, n) -> if x = j + c then termShift c s else TmVar(x, n)
    | TmAbs(x, ty, t) -> TmAbs(x, ty, walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
    | TmLet(n, t1, t2) -> TmLet(n, walk c t1, walk (c+1) t2)
    | TmTuple(fields) -> TmTuple(List.map (walk c) fields)
    | TmProj(t, i) -> TmProj(walk c t, i)
  in walk 0 t

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval t = match t with
  | TmTrue -> true
  | TmFalse -> true
  | TmUnit -> true
  | t when isnumericval t -> true
  | TmAbs(_, _, _) -> true
  | TmTuple(fields) -> List.for_all isval fields
  | _ -> false

exception NoRuleApplies

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
  | TmTuple(fields) -> "{" ^ (String.concat ", " (List.map (printtm ctx) fields)) ^ "}"
  | TmProj(t, i) -> printtm ctx t ^ "." ^ string_of_int i

let rec evalStep ctx t = match t with
  | TmIf(TmTrue, t2, _) -> t2
  | TmIf(TmFalse, _, t3) -> t3
  | TmIf(t1, t2, t3) -> let t1' = evalStep ctx t1 in TmIf(t1', t2, t3)
  | TmSucc(t1) -> let t1' = evalStep ctx t1 in TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
  | TmPred(t1) -> let t1' = evalStep ctx t1 in TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
  | TmIsZero(t1) -> let t1' = evalStep ctx t1 in TmIsZero(t1')
  | TmApp(TmAbs(_, _, t), v2) when isval v2 -> termSubstTop v2 t
  | TmApp(v1, t2) when isval v1 -> let t2' = evalStep ctx t2 in TmApp(v1, t2')
  | TmApp(t1, t2) -> let t1' = evalStep ctx t1 in TmApp(t1', t2)
  | TmLet(_, v1, t2) when isval v1 -> termSubstTop v1 t2
  | TmLet(n, t1, t2) -> let t1' = evalStep ctx t1 in TmLet(n, t1', t2)
  | TmTuple(fields) ->
      let rec evalnextfield l = match l with
        | [] -> raise NoRuleApplies
        | v::fs when isval v -> let vs = evalnextfield fs in v::vs
        | f::fs -> let v = evalStep ctx f in v::fs
      in let fields' = evalnextfield fields in
      TmTuple(fields')
  | TmProj(TmTuple(fields) as t, i) when isval t ->
      (try List.nth fields (i - 1)
      with Failure _ ->  raise NoRuleApplies)
  | TmProj(t, i) -> let t' = evalStep ctx t in TmProj(t', i)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try let t' = evalStep ctx t
    in eval ctx t'
  with NoRuleApplies -> t

exception TypeError

let rec typeof (ctx: context) (t: term) = match t with
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmZero -> TyNat
  | TmUnit -> TyUnit
  | TmPred(t1) when ((=) (typeof ctx t1) TyNat) -> TyNat
  | TmSucc(t1) when ((=) (typeof ctx t1) TyNat) -> TyNat
  | TmIsZero(t1) when ((=) (typeof ctx t1) TyNat) -> TyBool
  | TmIf(t1, t2, t3) ->
      let ty2 = typeof ctx t2 in
      let ty3 = typeof ctx t3 in
      if (=) ty2 ty3 && (=) (typeof ctx t1) TyBool then ty2 else (raise TypeError)
  | TmVar(i, _) -> (match getbinding ctx i with
      | (_, NameBind) -> raise TypeError
      | (_, VarBind(ty)) -> ty)
  | TmAbs(x, ty1, t) ->
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      let ty2 = typeof ctx' t in
      TyArr(ty1, ty2)
  | TmApp(t1, t2) ->
      let ty1 = typeof ctx t1 in
      let ty2 = typeof ctx t2 in
      (match ty1 with
        | TyArr(ty11, ty12) -> if (=) ty11 ty2 then ty12 else (raise TypeError)
        | _ -> raise TypeError)
  | TmLet(x, t1, t2) ->
      let ty1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      typeof ctx' t2
  | TmTuple(fields) -> TyTuple(List.map (typeof ctx) fields)
  | TmProj(t, i) ->
      (match typeof ctx t with
        | TyTuple(types) -> try List.nth types (i-1) with Failure _ -> raise TypeError
        | _ -> raise TypeError)
  | _ -> raise TypeError

let rec printty ty = match ty with
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyUnit -> "Unit"
  | TyTuple(tys) -> String.concat " * " (List.map printty tys)
  | TyArr(ty1, ty2) -> printty ty1 ^ " -> " ^ printty ty2
