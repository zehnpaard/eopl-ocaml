type t

val empty : t
val extend : t -> Type.t -> Type.t -> t
val apply : t -> Type.t -> Type.t
