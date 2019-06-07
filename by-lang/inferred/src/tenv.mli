type t

val empty : t
val find : t -> string -> Type.t option
val extend : t -> string -> Type.t -> t
