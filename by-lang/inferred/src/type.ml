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

let rec no_occurrence t1 = function
  | Int | Bool -> true
  | Var _ as t2 -> not (t1 = t2)
  | Proc (t2, t3) ->
      (no_occurrence t1 t2) && (no_occurrence t1 t3)
  | Unknown -> failwith "Invalid no occurrence check against Unknown type"
