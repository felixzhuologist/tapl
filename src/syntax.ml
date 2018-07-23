type term = 
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec printtm t = match t with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"
  | _ -> "TODO"
