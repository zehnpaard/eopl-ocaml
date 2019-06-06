type t =
  | Const of int
  | Var of string
  | ZeroP of t
  | Diff of t * t
  | If of t * t * t
  | Let of string * t * t
  | Proc of string * Type.t * t
  | Call of t * t
  | LetRec of Type.t * string * string * Type.t * t * t
