type t = Int
       | Bool
       | Proc of t * t
       | Unknown
       | Var of int

val init : unit -> unit
val free : unit -> t
val make_concrete : t -> t
val no_occurrence : t -> t -> bool