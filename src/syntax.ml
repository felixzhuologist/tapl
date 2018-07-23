type term = 
  | TmTrue
  | TmFalse
  | TmZero
  | TmIf of term * term * term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec printtm t = match t with
  | TmIf(_, _, _) -> "ifelse"
  | TmSucc(t) -> "succ " ^ printtm t
  | TmPred(t) -> "pred " ^ printtm t
  | TmIsZero(t) -> "iszero " ^ printtm t
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"

let rec isnumericval = function
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let isval = function
  | TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

exception NoRuleApplies

let rec eval_small_step = function
  (* fig 3.1 *)
  | TmIf(TmTrue, t2, _) -> t2
  | TmIf(TmFalse, _, t3) -> t3
  | TmIf(t1, t2, t3) -> let t1' = eval_small_step t1 in TmIf(t1', t2, t3)
  (* fig 3.2 *)
  | TmSucc(t1) -> let t1' = eval_small_step t1 in TmSucc(t1')
  | TmPred(TmZero) -> TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1
  | TmPred(t1) -> let t1' = eval_small_step t1 in TmPred(t1')
  | TmIsZero(TmZero) -> TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse
  | TmIsZero(t1) -> let t1' = eval_small_step t1 in TmIsZero(t1')
  | _ -> raise NoRuleApplies

let rec eval_big_step = function
  | t when isval t -> t
  | TmIf(t1, t2, t3) -> match eval_big_step t1 with
    | TmTrue -> t2
    | TmFalse -> t3
    | _ -> raise NoRuleApplies
  | TmSucc(t1) -> match eval_big_step t1 with
    | t when isnumericval t -> TmSucc(t)
    | _ -> raise NoRuleApplies
  | TmPred(t1) -> match eval_big_step t1 with
    | TmZero -> TmZero
    | TmSucc(nv1) when isnumericval nv1 -> nv1
    | _ -> raise NoRuleApplies
  | TmIsZero(t1) -> match eval_big_step t1 with
    | TmZero -> TmTrue
    | TmSucc(nv1) when isnumericval nv1 -> TmFalse
    | _ -> raise NoRuleApplies

let rec eval t =
  try let t' = eval_small_step t in eval t'
  with NoRuleApplies -> t
