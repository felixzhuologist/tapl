open Context
open Syntax

exception TypeError

(* Get the type for a given term under a context *)
val typeof : context -> term -> ty
