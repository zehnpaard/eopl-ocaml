type t =
  | Const of int
  | Var of string
  | ZeroP of t
  | Diff of t * t
  | If of t * t * t
  | Let of string * t * t
  | Proc of string * Opttype.t * t
  | Call of t * t
  | LetRec of Opttype.t * string * string * Opttype.t * t * t
