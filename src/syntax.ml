type ty =
  | TyBool
  | TyNat
  | TyUnit
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyArr of ty * ty
  | TyRef of ty

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
  | TmRecord of (string * term) list
  | TmProj of term * string
  | TmAscribe of term * ty
  | TmTag of string * term * ty
  | TmCase of term * (string * (string * term)) list
  | TmFix of term
  | TmRef of term
  | TmLoc of int
  | TmDeref of term
  | TmAssign of term * term

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

type store = term list

let (emptystore: store) = []

let extendstore store v = (List.length store, List.append store [v])

let lookuploc store i = List.nth store i

let rec updatestore store i v = match (i, store) with
  | (0, (_::vs)) -> v :: vs
  | (i, (v'::vs)) -> v' :: (updatestore vs (i-1) v)
  | _ -> raise LookupFailure

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
  | TmTag(_, t, _) -> isval t
  | TmLoc(_) -> true
  | _ -> false

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
        | TyArr(ty11, ty12) -> (if (=) ty11 ty2 then ty12 else (raise TypeError))
        | _ -> raise TypeError)
  | TmLet(x, t1, t2) ->
      let ty1 = typeof ctx t1 in
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      typeof ctx' t2
  | TmRecord(fields) ->
      TyRecord(List.map (fun (label, field) -> (label, (typeof ctx field))) fields)
  | TmProj(t, l) ->
      (match typeof ctx t with
        | TyRecord(types) -> (try List.assoc l types with Not_found -> raise TypeError)
        | _ -> raise TypeError)
  | TmAscribe(t, ty) ->
      let actual = typeof ctx t in
      if (=) actual ty then ty else raise TypeError
  | TmTag(label, t, (TyVariant(variants) as ty)) ->
      let expected = try List.assoc label variants with Not_found -> raise TypeError in
      if (=) expected (typeof ctx t) then ty else raise TypeError
  | TmCase(t, cases) ->
      (match typeof ctx t with
        | TyVariant(variants) ->
            let get_type (n, (x, t)) =
              let variant_type = try
                List.assoc n variants with
                Not_found -> raise TypeError in
              typeof (addbinding ctx x (VarBind(variant_type))) t in
            let result_types = List.map get_type cases in
            (* TODO: check that all result types are the same *)
            (* non emptiness should be enforced by the parser *)
            List.hd result_types
        | _ -> raise TypeError) (* case statements only work on variants for now *)
  | TmFix(t) ->
      (match typeof ctx t with
        | TyArr(ty1, ty2) -> if (=) ty1 ty2 then ty1 else raise TypeError
        | _ -> raise TypeError)
  | TmRef(_) | TmLoc(_) -> TyRef(TyUnit)
  | TmAssign(_, _) -> TyUnit
  | TmDeref(_) -> TyUnit
  | _ -> raise TypeError

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
    | TmTag(s, t, ty) -> TmTag(s, walk c t, ty)
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
    | TmTag(s, t, ty) -> TmTag(s, walk c t, ty)
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
  | TmTag(s, t, _) -> "<" ^ s ^ "=" ^ printtm ctx t ^ ">"
  | TmFix(t) -> "fix " ^ printtm ctx t
  | TmRef(t) -> "ref " ^ printtm ctx t
  | TmLoc(i) -> "<loc #" ^ string_of_int i ^ ">"
  | _ -> "TODO"

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
        | TmTag(label, value, _) ->
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
