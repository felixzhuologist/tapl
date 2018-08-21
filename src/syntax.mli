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
