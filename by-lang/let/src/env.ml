type t = (string * Val.t) list

let empty = []

let rec find env s = match env with
  | [] -> None
  | (s', v')::env' -> if s = s' then Some v' else find env' s

let extend env s v = (s, v)::env
