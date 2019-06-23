type t =
  | Const of int
  | Var of string
  | ZeroP of t
  | Diff of t * t
  | If of t * t * t
  | Let of string * t * t
  | Proc of string * t
  | Call of t * t
  | LetRec of string * string * t * t
  | Set of string * t
  | Block of t list
