open Context
open Syntax

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
  | TmRef(t) -> TyRef(typeof ctx t)
  | TmAssign(t1, t2) ->
      (match typeof ctx t1 with
        | TyRef(ty) -> if (=) ty (typeof ctx t2) then TyUnit else raise TypeError
        | _ -> raise TypeError)
  | TmDeref(t) ->
      (match typeof ctx t with
        | TyRef(ty) -> ty
        | _ -> raise TypeError)
  | _ -> raise TypeError
