type t =
  | Const of int
  | Var of string
  | If of t * t * t
  | Proc of string list * t
  | Call of t * t list

let rec to_string = function
| Const n -> string_of_int n
| Var s -> s
| If(x,y,z) -> Printf.sprintf "(if %s %s %s)" (to_string x) (to_string y) (to_string z)
| Proc(ss,t) -> Printf.sprintf "(proc [%s] %s)" (String.concat " " ss) (to_string t)
| Call(t,ts) -> Printf.sprintf "(%s %s)" (to_string t) (String.concat " " @@ List.map to_string ts)