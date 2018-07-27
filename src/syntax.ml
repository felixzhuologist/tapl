type term = 
  | TmVar of int * int
  | TmAbs of term
  | TmApp of term * term

let rec printtm t = match t with
  | TmVar(_,i) -> string_of_int i
  | TmAbs(t) -> "λ." ^ printtm t
  | TmApp(t1, t2) -> printtm t1 ^ " " ^ printtm t2

let eval t = t
