open Syntax

exception StoreLookupFailure

type store = term list

let (emptystore: store) = []

let extendstore store v = (List.length store, List.append store [v])

let lookuploc store i = List.nth store i

let rec updatestore store i v = match (i, store) with
  | (0, (_::vs)) -> v :: vs
  | (i, (v'::vs)) -> v' :: (updatestore vs (i-1) v)
  | _ -> raise StoreLookupFailure
