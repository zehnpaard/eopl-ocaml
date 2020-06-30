type t =
  | Const of int
  | Var of string
  | ZeroP of t
  | Diff of t * t
  | If of t * t * t
  | Let of (string * t) list * t
  | Proc of string list * t
  | Call of t * t list
  | LetRec of string * string * t * t
