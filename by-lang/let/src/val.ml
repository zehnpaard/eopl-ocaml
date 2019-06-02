type t = Num of int | Bool of bool

let to_str = function
  | Num n -> string_of_int n
  | Bool b -> if b then "True" else "False"
