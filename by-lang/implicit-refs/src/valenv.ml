type valt = Num of int
          | Bool of bool
          | Proc of string * Exp.t * envt
and envt = Empty
         | Extend of string * int * envt
 
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
    | Extend (s', n', env') ->
        if s = s' then Some n'
        else find env' s
  
  let extend env s n = Extend (s, n, env)
end
