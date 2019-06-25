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
  val find : t -> string -> int option
  val extend : t -> string -> int -> t
end
