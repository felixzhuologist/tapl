open Syntax
open Store

let rec isnumericval t = match t with
  | TmZero -> true
  | TmSucc(t1) -> isnumericval t1 
  | _ -> false

let rec isval t = match t with
  | TmTrue -> true
  | TmFalse -> true
  | TmUnit -> true
  | t when isnumericval t -> true
  | TmAbs(_, _, _) -> true
  | TmRecord(fields) -> List.for_all isval (List.map (fun (_, f) -> f) fields)
  | TmAscribe(t, _) -> isval t
  | TmTag(_, t) -> isval t
  | TmLoc(_) -> true
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
    | TmRecord(fields) -> TmRecord(List.map (fun (l, f) -> (l, walk c f)) fields)
    | TmProj(t, l) -> TmProj(walk c t, l)
    | TmAscribe(t, ty) -> TmAscribe(walk c t, ty)
    | TmTag(s, t) -> TmTag(s, walk c t)
    | TmCase(t, cases) ->
        let cases' = List.map (fun (x, (y, t)) -> (x, (y, walk c t))) cases in
        TmCase(walk c t, cases')
    | TmFix(t) -> TmFix(walk c t)
    | TmRef(t) -> TmRef(walk c t)
    | TmLoc(_) as t -> t
    | TmDeref(t) -> TmDeref(walk c t)
    | TmAssign(t1, t2) -> TmAssign(walk c t1, walk c t2)
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
    | TmRecord(fields) -> TmRecord(List.map (fun (l, f) -> (l, walk c f)) fields)
    | TmProj(t, l) -> TmProj(walk c t, l)
    | TmAscribe(t, ty) -> TmAscribe(walk c t, ty)
    | TmTag(s, t) -> TmTag(s, walk c t)
    | TmCase(t, cases) ->
        let cases' = List.map (fun (x, (y, t)) -> (x, (y, walk c t))) cases in
        TmCase(walk c t, cases')
    | TmFix(t) -> TmFix(walk c t)
    | TmRef(t) -> TmRef(walk c t)
    | TmLoc(_) as t -> t
    | TmDeref(t) -> TmDeref(walk c t)
    | TmAssign(t1, t2) -> TmAssign(walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

exception NoRuleApplies

let rec evalStep ctx store t = match t with
  | TmIf(TmTrue, t2, _) -> t2, store
  | TmIf(TmFalse, _, t3) -> t3, store
  | TmIf(t1, t2, t3) ->
      let (t1', store') = evalStep ctx store t1 in TmIf(t1', t2, t3), store'
  | TmSucc(t1) ->
      let (t1', store') = evalStep ctx store t1 in TmSucc(t1'), store'
  | TmPred(TmZero) -> TmZero, store
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) -> nv1, store
  | TmPred(t1) ->
      let (t1', store') = evalStep ctx store t1 in TmPred(t1'), store'
  | TmIsZero(TmZero) -> TmTrue, store
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) -> TmFalse, store
  | TmIsZero(t1) ->
      let (t1', store') = evalStep ctx store t1 in TmIsZero(t1'), store'
  | TmApp(TmAbs(_, _, t), v2) when isval v2 -> termSubstTop v2 t, store
  | TmApp(v1, t2) when isval v1 ->
      let (t2', store') = evalStep ctx store t2 in TmApp(v1, t2'), store'
  | TmApp(t1, t2) ->
      let (t1', store') = evalStep ctx store t1 in TmApp(t1', t2), store'
  | TmLet(_, v1, t2) when isval v1 -> termSubstTop v1 t2, store
  | TmLet(n, t1, t2) ->
      let (t1', store') = evalStep ctx store t1 in TmLet(n, t1', t2), store'
  | TmRecord(fields) ->
      let rec evalnextfield l = match l with
        | [] -> raise NoRuleApplies
        | (l,v)::fs when isval v -> let (vs, s) = evalnextfield fs in ((l,v)::vs, s)
        | (l,f)::fs -> let (v, s) = evalStep ctx store f in ((l,v)::fs, s)
      in let (fields', store') = evalnextfield fields in
      TmRecord(fields'), store'
  | TmProj(TmRecord(fields) as t, l) when isval t ->
      (try List.assoc l fields, store
      with Not_found ->  raise NoRuleApplies)
  | TmProj(t, l) ->
      let (t', store') = evalStep ctx store t in TmProj(t', l), store'
  | TmAscribe(t, _) -> t, store
  | TmCase(t, cases) when isval t ->
      (match t with
        | TmTag(label, value) ->
            (let (_, case) =
              try List.assoc label cases
              with Not_found -> raise NoRuleApplies
            in
            termSubstTop value case), store
        | _ -> raise NoRuleApplies)
  | TmCase(t, cases) ->
      let (t', store') = evalStep ctx store t in TmCase(t', cases), store'
  | TmFix(t1) as t when isval t1 ->
      (match t1 with
        | TmAbs(_, _, t12) -> termSubstTop t t12, store
        | _ -> raise NoRuleApplies)
  | TmFix(t) ->
      let (t', store') = evalStep ctx store t in TmFix(t'), store'
  | TmRef(v) when isval v ->
      let (loc, store') = extendstore store v in TmLoc(loc), store'
  | TmRef(t) ->
      let (t', store') = evalStep ctx store t in TmRef(t'), store'
  | TmDeref(v) when isval v ->
      (match v with
        | TmLoc(loc) -> lookuploc store loc, store
        | _ -> raise NoRuleApplies)
  | TmDeref(t) ->
      let (t', store') = evalStep ctx store t in TmDeref(t'), store' 
  | TmAssign(v1, v2) when isval v1 && isval v2 ->
      (match v1 with
        | TmLoc(loc) -> TmUnit, (updatestore store loc v2)
        | _ -> raise NoRuleApplies)
  | TmAssign(v1, t2) when isval v1 ->
      let (t2', store') = evalStep ctx store t2 in TmAssign(v1, t2'), store'
  | TmAssign(t1, t2) ->
      let (t1', store') = evalStep ctx store t1 in TmAssign(t1', t2), store'
  | _ -> raise NoRuleApplies

let rec eval ctx store t =
  try let (t', store') = evalStep ctx store t
    in eval ctx store' t'
  with NoRuleApplies -> t, store
