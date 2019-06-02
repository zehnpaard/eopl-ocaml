type t

val empty : t

val find : t -> string -> Val.t option
val extend : t -> string -> Val.t -> t
