type envt
type valt = Num of int
          | Bool of bool
          | Proc of string * Exp.t * envt

module Val : sig
  type t = valt = Num of int
                | Bool of bool
                | Proc of string * Exp.t * envt
  val to_str : t -> string
end

module Env : sig
  type t = envt
  val empty : t
  val find : t -> string -> valt option
  val extend : t -> string -> valt -> t
  val extend_rec : t -> string -> string -> Exp.t -> t
end
