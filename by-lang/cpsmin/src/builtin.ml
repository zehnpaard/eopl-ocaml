let num_num_op s op =
  let g = function
  | Val.Num n -> n
  | _ -> failwith @@ "Non-numeric value passed to numeric op " ^ s
  in
  let f xs = match List.map g xs with
  | [] -> failwith @@ Printf.sprintf "Numeric op %s applied to empty list" s
  | y::ys -> Val.Num (List.fold_left op y ys)
  in
  Val.Op(s, f)

let num_bool_op s op =
  let g = function
  | Val.Num n -> n
  | _ -> failwith @@ "Non-numeric value passed to numeric op " ^ s
  in
  let rec h op = function
  | [] | [_] -> Val.Bool true
  | x::y::ys -> if op x y then h op @@ y::ys else Val.Bool false
  in
  let f xs = match List.map g xs with
  | [] -> failwith @@ Printf.sprintf "NumBool op %s applied to empty list" s
  | [_] -> failwith @@ Printf.sprintf "NumBool op %s applied to one arg" s
  | ys -> h op ys
  in
  Val.Op(s, f)

let zero_op = function
| [Val.Num n] -> Val.Bool(n = 0)
| _ -> failwith "Incorrect argument passed to zero?"

let builtins =
[ "+", num_num_op "+" (+)
; "-", num_num_op "-" (-)
; "*", num_num_op "*" ( * )
; "/", num_num_op "/" (/)
; "true", Val.Bool true
; "false", Val.Bool false
; "zero?", Val.Op("zero?",zero_op)
]

let load env =
  let ss, es = List.split builtins in
  Env.extend_list env ss es