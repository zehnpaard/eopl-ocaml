type t = (string * Type.t) list

let empty = []
let find tenv s = List.assoc_opt s tenv
let extend tenv s t = (s, t)::tenv
