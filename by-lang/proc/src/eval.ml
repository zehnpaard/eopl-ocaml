let rec eval' env = function
  | Exp.Const n -> Val.Num n
  | Exp.Var s -> (match Env.find env s with
      | Some v -> v
      | None -> failwith "Variable not found in environment")
  | Exp.ZeroP e -> (match eval' env e with
      | Val.Num n -> Val.Bool (n = 0)
      | _ -> failwith "Zero-checking non-numeric value")
  | Exp.Diff (e1, e2) -> (match eval' env e1, eval' env e2 with
      | Val.Num n1, Val.Num n2 -> Val.Num (n1 - n2)
      | _ -> failwith "Diffing non-numeric value(s)")
  | Exp.If (e1, e2, e3) -> (match eval' env e1 with
      | Val.Bool b -> eval' env (if b then e2 else e3)
      | _ -> failwith "Using non-boolean if-condition")
  | Exp.Let (s1, e1, e2) ->
      let v1 = eval' env e1 in
      let env' = Env.extend env s1 v1 in
      eval' env' e2
  | Exp.Proc (s, e) -> Val.Proc (s, e, env)
  | Exp.Call (e1, e2) -> (match eval' env e1 with
      | Val.Proc (s, e, env') ->
          let v = eval' env e2 in
          let env'' = Env.extend env' s v in
          eval' env'' e
      | _ -> failwith "Calling non-procedure")

let f = eval' Env.empty
