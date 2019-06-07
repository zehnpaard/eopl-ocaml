type t = (string * Type.t) list

let empty = []
let rec find tenv s = match tenv with
  | [] -> None
  | (s', t')::tenv' -> if s = s' then Some t' else find tenv' s
let extend tenv s t = (s, t)::tenv
  
