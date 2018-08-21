open Syntax

(* Get display string for a type *)
val printty : ty -> string

(* Get display string for a term *)
val printtm : Context.context -> term -> string
