type t = Int
       | Bool
       | Proc of t * t
       | Unknown
       | Var of int

val init : unit -> unit
val new_var : unit -> t
val make_concrete : t -> t
val occurs : t -> t -> bool
