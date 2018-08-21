type binding =
  | NameBind
  | VarBind of Syntax.ty

(* Context storing the current bindings in scope *)
type context

(* The empty context *)
val emptycontext : context

(* Get the size of the context *)
val ctxlength : context -> int

(* Add a new binding to the context *)
val addbinding : context -> string -> binding -> context

(* Return the name and binding for a de Bruijn index *)
val getbinding : context -> int -> (string * binding)

(* Get the de Bruijn index by name *)
val name2index : context -> string -> int

(* Check if a given name exists in the context *)
val isnamebound : context -> string -> bool

(* Given a name, generate a similar one that doesn't exist
 * in the context (if necessary), then add it to the context  *)
val pickfreshname : context -> string -> (context * string)
