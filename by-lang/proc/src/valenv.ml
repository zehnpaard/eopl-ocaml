module rec Val : sig
  type t = Num of int
         | Bool of bool
         | Proc of string * Exp.t * Env.t

  val to_str : t -> string
end = struct
  type t = Num of int
         | Bool of bool
         | Proc of string * Exp.t * Env.t

  let to_str = function
    | Num n -> string_of_int n
    | Bool b -> if b then "True" else "False"
    | Proc _ -> "Proc"
end

and Env : sig
  type t

  val empty : t
  val find : t -> string -> Val.t option
  val extend : t -> string -> Val.t -> t
end = struct
  type t = (string * Val.t) list

  let empty = []
  
  let rec find env s = match env with
    | [] -> None
    | (s', v')::env' -> if s = s' then Some v' else find env' s
  
  let extend env s v = (s, v)::env
end
