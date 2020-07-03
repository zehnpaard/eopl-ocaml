type t =
  | Const of int
  | Var of string
  | ZeroP of t
  | If of t * t * t
  | Let of (string * t) list * t
  | Proc of string list * t
  | Call of t * t list
  | LetRec of (string * string list * t) list * t
