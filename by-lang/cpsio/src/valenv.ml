type valt = Num of int
          | Bool of bool
          | Proc of string list * Exp.t * envt
and envt = Empty
         | Extend of string * valt * envt
         | ExtendRec of (string * (string list * Exp.t)) list * envt
 
module Val = struct
  type t = valt = Num of int
                | Bool of bool
                | Proc of string list * Exp.t * envt

  let to_str = function
    | Num n -> string_of_int n
    | Bool b -> if b then "True" else "False"
    | Proc _ -> "Proc"
end

module Env = struct
  type t = envt

  let empty = Empty
  
  let rec find env s = match env with
    | Empty -> None
    | Extend (s', v', env') ->
        if s = s' then Some v'
        else find env' s
    | ExtendRec (fns, env') -> (match List.assoc_opt s fns with
      | None -> find env' s
      | Some(args, body) -> Some (Val.Proc(args, body, env)))
  
  let extend env s v = Extend (s, v, env)
  let rec extend_list env ss vs = match ss, vs with
  | [], [] -> env
  | [], _ | _, [] -> failwith ""
  | (s::ss), (v::vs) -> extend_list (Extend(s,v,env)) ss vs

  let extend_rec env f a b = ExtendRec ([(f,(a,b))], env)
  let extend_rec_list env fs xs ys =
    let xys = List.combine xs ys in
    let fns = List.combine fs xys in
    ExtendRec(fns,env)
end
