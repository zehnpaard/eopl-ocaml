type env = (string * Type.t) list
let empty = []
let rec find tenv s = match tenv with
  | [] -> None
  | (s', t')::tenv' -> if s = s' then Some t' else find tenv' s
let extend tenv s t = (s, t)::tenv
  
let rec check tenv = function
  | Exp.Const _ -> Type.Int
  | Exp.Var s -> (match find tenv s with
      | Some t -> t
      | None -> failwith "Variable not typed")
  | Exp.ZeroP e -> (match check tenv e with
      | Type.Int -> Type.Bool
      | _ -> failwith "Non-numeric type passed to zero?")
  | Exp.Diff (e1, e2) -> (match check tenv e1, check tenv e2 with
      | Type.Int, Type.Int -> Type.Int
      | _ -> failwith "Non-numeric type passed to -(x,y)")
  | Exp.If (e1, e2, e3) -> (match check tenv e1 with
      | Type.Bool ->
          let t = check tenv e2 in
          if t = check tenv e3 then t
          else failwith "Different types in if then and else clauses"
      | _ -> failwith "Non-boolean passed as if condition")
  | Exp.Let (s1, e1, e2) ->
      let t1 = check tenv e1 in
      let tenv' = extend tenv s1 t1 in
      check tenv' e2
  | Exp.Proc (s, t, e) ->
      let tenv' = extend tenv s t in
      let rettype = check tenv' e in
      Type.Proc (t, rettype)
  | Exp.Call (e1, e2) -> (match check tenv e1 with
      | Type.Proc (t1, t2) ->
          if t1 = check tenv e2 then t2
          else failwith "Proc signature and argument types are mismatched"
      | _ -> failwith "Non-proc type in proc position of call")
  | Exp.LetRec (t1, fname, _, t2, _, e) ->
      let t' = Type.Proc(t2, t1) in
      let tenv' = extend tenv fname t' in
      check tenv' e

let f = check empty
