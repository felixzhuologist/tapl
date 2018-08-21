open Syntax

(* store over a set of locations containing reference values *)
type store

(* the empty store *)
val emptystore : store

(* add a new term to the store and return the location and new store *)
val extendstore : store -> term -> (int * store)

(* get the value at a location in the store *)
val lookuploc : store -> int -> term

(* update the value at a location in the store *)
val updatestore : store -> int -> term -> store
