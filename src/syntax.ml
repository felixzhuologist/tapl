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
  | _ -> "TODO"
