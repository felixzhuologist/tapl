type term = 
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmZero
  | TmIf of term * term * term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

type binding = NameBind

exception LookupFailure

type context = (string * binding) list

let emptycontext = []

let ctxlength ctx = List.length ctx

let addname ctx name bind = (name, bind) :: ctx

let index2name ctx x =
  try
    let (xn, _) = List.nth ctx x in xn
  with Failure _ ->
    raise LookupFailure

let rec name2index ctx x =
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
    | TmIf(t1, t2, t3) -> TmIf(walk c t1, walk c t2, walk c t3)
    | TmSucc(t1) -> TmSucc(walk c t1)
    | TmPred(t1) -> TmPred(walk c t1)
    | TmIsZero(t1) -> TmIsZero(walk c t1)
    | TmVar(x, n) -> if x >= c then TmVar(x + d, n + d) else TmVar(x, n + d)
    | TmAbs(x, t) -> TmAbs(x, walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in walk 0 t

(* [ j -> s ]t *)
let termSubst j s t =
  let rec walk c t = match t with
    | TmTrue -> TmTrue
    | TmFalse -> TmFalse
    | TmZero -> TmZero
    | TmIf(t1, t2, t3) -> TmIf(walk c t1, walk c t2, walk c t3)
    | TmSucc(t1) -> TmSucc(walk c t1)
    | TmPred(t1) -> TmPred(walk c t1)
    | TmIsZero(t1) -> TmIsZero(walk c t1)
    | TmVar(x, n) -> if x = j + c then termShift c s else TmVar(x, n)
    | TmAbs(x, t) -> TmAbs(x, walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

let isval t = match t with
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | TmAbs(_, _) -> true
  | _ -> false

exception NoRuleApplies

let rec printtm t = match t with
  | TmVar(_,i) -> string_of_int i
  (* | TmAbs(x, t) -> let (ctx', x') = *)
  | TmAbs(x, t) -> ("λ." ^ printtm t)
  | TmApp(t1, t2) -> printtm t1 ^ " " ^ printtm t2
  | TmIf(_, _, _) -> "ifelse"
  | TmSucc(t) -> "succ " ^ printtm t
  | TmPred(t) -> "pred " ^ printtm t
  | TmIsZero(t) -> "iszero " ^ printtm t
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"

let rec evalStep t = match t with
  | TmIf(TmTrue, t2, _) -> t2
  | TmIf(TmFalse, _, t3) -> t3
  | TmIf(t1, t2, t3) -> let t1' = evalStep t1 in TmIf(t1', t2, t3)
  | TmSucc(t1) -> let t1' = evalStep t1 in TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
  | TmPred(t1) -> let t1' = evalStep t1 in TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
  | TmIsZero(t1) -> let t1' = evalStep t1 in TmIsZero(t1')
  | TmApp(TmAbs(x, t), v2) when isval v2 -> termSubstTop v2 t
  | TmApp(v1, t2) when isval v1 -> let t2' = evalStep t2 in TmApp(v1, t2')
  | TmApp(t1, t2) -> let t1' = evalStep t1 in TmApp(t1', t2)
  | _ -> raise NoRuleApplies

let rec eval t =
  try let t' = evalStep t
    in eval t'
  with NoRuleApplies -> t
