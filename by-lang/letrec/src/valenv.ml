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
  type t = Empty
         | Extend of string * Val.t * t
         | ExtendRec of string * string * Exp.t * t

  val empty : t
  val find : t -> string -> Val.t option
  val extend : t -> string -> Val.t -> t
  val extend_rec : t -> string -> string -> Exp.t -> t
end = struct
  type t = Empty
         | Extend of string * Val.t * t
         | ExtendRec of string * string * Exp.t * t

  let empty = Empty
  
  let rec find env s = match env with
    | Empty -> None
    | Extend (s', v', env') ->
        if s = s' then Some v'
        else find env' s
    | ExtendRec (fname, arg, body, env') ->
        if s = fname then Some (Val.Proc (arg, body, env)) 
        else find env' s 
  
  let extend env s v = Extend (s, v, env)
  let extend_rec env f a b = ExtendRec (f, a, b, env)
end
