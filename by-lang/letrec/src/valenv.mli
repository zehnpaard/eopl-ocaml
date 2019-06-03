module rec Val : sig
  type t = Num of int
         | Bool of bool
         | Proc of string * Exp.t * Env.t
  val to_str : t -> string
end
and Env : sig
  type t
  val empty : t
  val find : t -> string -> Val.t option
  val extend : t -> string -> Val.t -> t
end
