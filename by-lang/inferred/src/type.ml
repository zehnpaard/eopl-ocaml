type t = Int
       | Bool
       | Proc of t * t
       | Unknown
       | Var of int

let n = ref 0
let init () = n := 0
let new_var () = let x = !n in (incr n; Var x)

let make_concrete = function
  | Unknown -> new_var ()
  | t -> t

let rec occurs t1 = function
  | Int | Bool -> false
  | Var _ as t2 -> t1 = t2
  | Proc (t2, t3) ->
      occurs t1 t2 || occurs t1 t3
  | Unknown -> failwith "Invalid no occurrence check against Unknown type"
