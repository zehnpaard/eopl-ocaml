module Val : sig
  type t = val
  val to_str : t -> string
end

module Env : sig
  type t = env
  val empty : t
  val find : t -> string -> Val.t option
  val extend : t -> string -> Val.t -> t
end
