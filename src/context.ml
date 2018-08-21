type binding =
  | NameBind
  | VarBind of Syntax.ty

exception ContextLookupFailure

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
    raise ContextLookupFailure

let rec name2index (ctx: context) (x: string) =
  match ctx with
    | [] -> raise ContextLookupFailure
    | (y,_)::rest -> if y=x then 0 else 1 + (name2index rest x)
