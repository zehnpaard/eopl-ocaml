type val = Num of int | Bool of bool
and env = (string * val) list


module Val : sig
  type t = val
  val to_str : t -> string
end = struct
  let to_str = function
    | Num n -> string_of_int n
    | Bool b -> if b then "True" else "False"
end


module Env : sig
  type t = env
  val empty : t
  val find : t -> string -> Val.t option
  val extend : t -> string -> Val.t -> t
end = struct
  let empty = []
  
  let rec find env s = match env with
    | [] -> None
    | (s', v')::env' -> if s = s' then Some v' else find env' s
  
  let extend env s v = (s, v)::env
end
