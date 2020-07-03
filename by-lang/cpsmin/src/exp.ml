type t =
  | Const of int
  | Var of string
  | If of t * t * t
  | Proc of string list * t
  | Call of t * t list
