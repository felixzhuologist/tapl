open Context
open Store
open Syntax

(* Evaluate term under given context and store as far as 
 * possible, then return result term and store *)
val eval : context -> store -> term -> (term * store)
