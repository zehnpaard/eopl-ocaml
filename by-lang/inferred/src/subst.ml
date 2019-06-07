type t = (Type.t * Type.t) list

let empty = []

let extend subst tv t = (tv, t)::subst 

let rec apply subst = function
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Proc (t1, t2) ->
      Type.Proc (apply subst t1, apply subst t2)
  | Type.Var _ as t1 -> (match List.assoc_opt t1 subst with
      | Some t2 -> t2
      | None -> t1)
  | Type.Unknown -> failwith "Invalid substitution application to Unknown type"
