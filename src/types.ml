open Context
open Syntax

exception TypeError

let rec (<:) ty1 ty2 =
  (=) ty1 ty2 ||
  match (ty1, ty2) with
    | (TyRecord(fields1), TyRecord(fields2)) ->
        let depthsub (label, fty2) =
          try let fty1 = List.assoc label fields1 in
            fty2 <: fty1
          with Not_found -> false
        in
        List.for_all depthsub fields2
    | (TyArr(s1, s2), TyArr(t1, t2)) ->
        t1 <: s1 && s2 <: t2
    | (TyVariant(fields1), TyVariant(fields2)) -> 
        let depthsub (label, fty1) =
          try let fty2 = List.assoc label fields2 in
            fty1 <: fty2
          with Not_found -> false
        in
        List.for_all depthsub fields1
    | (_, TyTop) -> true
    | _ -> false

let join ty1 ty2 =
  if ty1 <: ty2 then ty2 else
  if ty2 <: ty1 then ty1 else
  (* TODO: joins for records, functions, variants *)
  TyTop

type constr = (ty * ty) list
type tymap = (string * ty) list

let occursin (s: string) (ty: ty) : bool =
  let rec o ty = match ty with
    | TyId(s') -> s = s'
    | TyNat -> false
    | TyBool -> false
    | TyUnit -> false
    | TyTop -> false
    | TyArr(ty1, ty2) -> o ty1 || o ty2
    | TyRef(ty1) -> o ty1
    | TyRecord(fields) ->
        List.fold_left (||) false (List.map (fun (_, t) -> o t) fields)
    | TyVariant(fields) ->
        List.fold_left (||) false (List.map (fun (_, t) -> o t) fields)
  in o ty

(* subst ?Xi = T, into S *)
let sub_into_ty (s: string) (tyT: ty) (tyS: ty) : ty =
  let rec f tyS = match tyS with
    | TyId(s') -> if s = s' then tyT else TyId(s')
    | TyNat -> TyNat
    | TyBool -> TyBool
    | TyUnit -> TyUnit
    | TyTop -> TyTop
    | TyArr(ty1, ty2) -> TyArr(f ty1, f ty2)
    | TyRef(ty1) -> f ty1
    | TyRecord(fields) -> TyRecord(List.map (fun (l, t) -> (l, f t)) fields)
    | TyVariant(fields) -> TyVariant(List.map (fun (l, t) -> (l, f t)) fields)
  in f tyS

let sub_into_constr (s: string) (ty: ty) (constr: constr) : constr =
  List.map
    (fun (ty1, ty2) -> (sub_into_ty s ty ty1, sub_into_ty s ty ty2))
    constr

(* try picking ?X0 as the new name. If that's bound, try ?X1, then ?X2, and so on...
 * searches through all constraints for each new name which can be made more efficient *)
let pickfreshty (constr: constr): string =
  let istynamebound (s: string) (constr: constr) : bool =
    List.fold_left
      (fun acc (l, r) -> acc || (occursin s l) || (occursin s r))
      false constr
  in 
  let rec pick (i: int) : string =
    let name = "?X" ^ string_of_int i in
    if istynamebound name constr then pick (i+1) else name
  in
  pick 0

let rec unify (constr: constr) : tymap = match constr with
  | [] -> []
  | (TyId(s), TyId(s'))::rest when s = s' -> unify rest
  | (ty1, TyId(s))::rest ->
      if occursin s ty1 then []
      else List.append
        (unify (sub_into_constr s ty1 rest))
        [(s, ty1)]
  | (TyId(s), ty2)::rest ->
      if occursin s ty2 then []
      else List.append
        (unify (sub_into_constr s ty2 rest))
        [(s, ty2)]
  | (TyArr(ty11, ty12), TyArr(ty21, ty22))::rest ->
      unify ((ty11, ty21)::(ty12, ty22)::rest)
  | (ty1, ty2)::rest ->
      if ty1 <: ty2 || ty2 <: ty1 then unify rest
      else raise TypeError

let applysubst (sigma: tymap) (ty: ty) : ty =
    List.fold_left
      (fun ty (i, tyval) -> sub_into_ty i tyval ty)
      ty (List.rev sigma)

let rec typeof_non_poly (ctx: context) (t: term) : ty = match t with
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmZero -> TyNat
  | TmUnit -> TyUnit
  | TmPred(t1) when ((=) (typeof_non_poly ctx t1) TyNat) -> TyNat
  | TmSucc(t1) when ((=) (typeof_non_poly ctx t1) TyNat) -> TyNat
  | TmIsZero(t1) when ((=) (typeof_non_poly ctx t1) TyNat) -> TyBool
  | TmIf(t1, t2, t3) ->
      let ty2 = typeof_non_poly ctx t2 in
      let ty3 = typeof_non_poly ctx t3 in
      if (=) (typeof_non_poly ctx t1) TyBool then join ty2 ty3 else (raise TypeError)
  | TmVar(i, _) -> (match getbinding ctx i with
      | (_, NameBind) -> raise TypeError
      | (_, VarBind(ty)) -> ty)
  | TmAbs(x, ty1, t) ->
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      let ty2 = typeof_non_poly ctx' t in
      TyArr(ty1, ty2)
  | TmApp(t1, t2) ->
      let ty1 = typeof_non_poly ctx t1 in
      let ty2 = typeof_non_poly ctx t2 in
      (match ty1 with
        | TyArr(ty11, ty12) -> (if ty2 <: ty11 then ty12 else (raise TypeError))
        | _ -> raise TypeError)
  | TmLet(x, t1, t2) ->
      let ty1 = typeof_non_poly ctx t1 in
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      typeof_non_poly ctx' t2
  | TmRecord(fields) ->
      TyRecord(List.map (fun (label, field) -> (label, (typeof_non_poly ctx field))) fields)
  | TmProj(t, l) ->
      (match typeof_non_poly ctx t with
        | TyRecord(types) -> (try List.assoc l types with Not_found -> raise TypeError)
        | _ -> raise TypeError)
  | TmAscribe(t, ty) ->
      let actual = typeof_non_poly ctx t in
      if actual <: ty then ty else raise TypeError
  | TmTag(label, t) -> TyVariant([(label, typeof_non_poly ctx t)])
  | TmCase(t, cases) ->
      (match typeof_non_poly ctx t with
        | TyVariant(variants) ->
            let get_type (n, (x, t)) =
              let variant_type = List.assoc n variants in
              typeof_non_poly (addbinding ctx x (VarBind(variant_type))) t
            in
            (* TODO *)
            (cases
              |> List.filter (fun (n, _) -> List.mem_assoc n variants)
              |> List.map get_type
              |> List.hd)
        | _ -> raise TypeError)
  | TmFix(t) ->
      (match typeof_non_poly ctx t with
        | TyArr(ty1, ty2) -> if ty2 <: ty1 then ty2 else raise TypeError
        | _ -> raise TypeError)
  | TmRef(t) -> TyRef(typeof_non_poly ctx t)
  | TmAssign(t1, t2) ->
      (match typeof_non_poly ctx t1 with
        | TyRef(ty) -> if (=) ty (typeof_non_poly ctx t2) then TyUnit else raise TypeError
        | _ -> raise TypeError)
  | TmDeref(t) ->
      (match typeof_non_poly ctx t with
        | TyRef(ty) -> ty
        | _ -> raise TypeError)
  | _ -> raise TypeError

let rec get_constr (ctx: context) (t: term) : (ty * constr) = match t with
  | TmTrue -> (TyBool, [])
  | TmFalse -> (TyBool, [])
  | TmZero -> (TyNat, [])
  | TmUnit -> (TyUnit, [])
  | TmPred(t1) | TmSucc(t1) -> 
      let (ty, constr) = get_constr ctx t1 in
      (TyNat, (ty, TyNat)::constr)
  | TmIsZero(t1) ->
      let (ty, constr) = get_constr ctx t1 in
      (TyBool, (ty, TyNat)::constr)
  | TmIf(t1, t2, t3) ->
      let (ty1, constr1) = get_constr ctx t1 in
      let (ty2, constr2) = get_constr ctx t2 in
      let (ty3, constr3) = get_constr ctx t3 in
      let newconstr = [(ty1, TyBool)] in
      (join ty2 ty3, List.concat [newconstr; constr1; constr2; constr3])
  | TmVar(i, _) -> (match getbinding ctx i with
      | (_, NameBind) -> raise TypeError
      | (_, VarBind(ty)) -> (ty, []))
  | TmAbs(x, ty1, t) ->
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      let (ty2, constr) = get_constr ctx' t in
      (TyArr(ty1, ty2), constr)
  | TmApp(t1, t2) ->
      let (ty1, constr1) = get_constr ctx t1 in
      let (ty2, constr2) = get_constr ctx t2 in
      let constr' = constr1 @ constr2 in
      let x = pickfreshty constr' in
      (TyId(x), (ty1, TyArr(ty2, TyId(x)))::constr')
  | TmLet(x, t1, t2) ->
      let (ty1, constr1) = get_constr ctx t1 in
      let ctx' = addbinding ctx x (VarBind(ty1)) in
      let (ty2, constr2) = get_constr ctx' t2 in
      (ty2, constr1 @ constr2)
  (* TODO: polymorphism for records, cases, rec functions, refs *)
  | t -> (typeof_non_poly ctx t, [])

let typeof (ctx: context) (t: term) : ty =
  let (ty, constraints) = get_constr ctx t in
  let sigma = unify constraints in
  applysubst sigma ty
