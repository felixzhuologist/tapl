open Context
open Syntax

(* Get display string for a type *)
val printty : context -> ty -> string

(* Get display string for a term *)
val printtm : context -> term -> string
