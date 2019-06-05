type valt = Num of int
          | Bool of bool
          | Proc of string * Exp.t * envt
and envt = Empty
         | Extend of string * valt * envt
         | ExtendRec of string * string * Exp.t * envt
 
module Val = struct
  type t = valt = Num of int
                | Bool of bool
                | Proc of string * Exp.t * envt

  let to_str = function
    | Num n -> string_of_int n
    | Bool b -> if b then "True" else "False"
    | Proc _ -> "Proc"
end

module Env = struct
  type t = envt

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
