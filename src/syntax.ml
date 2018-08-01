type term = 
  | TmVar of int * int
  | TmAbs of term
  | TmApp of term * term

type binding = NameBind

type context = (string * binding) list

let termShift d t =
  let rec walk c t = match t with
    | TmVar(x, n) -> if x >= c then TmVar(x + d, n + d) else TmVar(x, n + d)
    | TmAbs(t) -> TmAbs(walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in walk 0 t

(* [ j -> s ]t *)
let termSubst j s t =
  let rec walk c t = match t with
    | TmVar(x, n) -> if x = j + c then termShift c s else TmVar(x, n)
    | TmAbs(t) -> TmAbs(walk (c + 1) t)
    | TmApp(t1, t2) -> TmApp(walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

let isval t = match t with
  | TmAbs(_) -> true
  | _ -> false

exception NoRuleApplies

let rec printtm t = match t with
  | TmVar(_,i) -> string_of_int i
  | TmAbs(t) -> "λ." ^ printtm t
  | TmApp(t1, t2) -> printtm t1 ^ " " ^ printtm t2

let rec evalStep t = match t with
  | TmApp(TmAbs(t),v2) when isval v2 -> termSubstTop v2 t
  | TmApp(v1, t2) when isval v1 -> let t2' = evalStep t2 in TmApp(v1, t2')
  | TmApp(t1, t2) -> let t1' = evalStep t1 in TmApp(t1', t2)
  | _ -> raise NoRuleApplies

let rec eval t =
  try let t' = evalStep t
    in eval t'
  with NoRuleApplies -> t
