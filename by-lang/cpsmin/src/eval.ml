let rec eval' env = function
  | Exp.Const n -> Val.Num n
  | Exp.Var s -> (match Env.find env s with
      | Some v -> v
      | None -> failwith "Variable not found in environment")
  | Exp.If (e1, e2, e3) -> (match eval' env e1 with
      | Val.Bool b -> eval' env (if b then e2 else e3)
      | _ -> failwith "Using non-boolean if-condition")
  | Exp.Proc (ss, e) -> Val.Proc (ss, e, env)
  | Exp.Call (e1, es) -> (match eval' env e1 with
      | Val.Proc (ss, e, env') ->
          let vs = List.map (eval' env) es in
          let env'' = Env.extend_list env' ss vs in
          eval' env'' e
      | Val.Op (_, f) -> (match List.rev es with
        | (Proc([s],e))::es ->
          let res = f (List.map (eval' env) (List.rev es)) in
          let env = Env.extend env s res in
          eval' env e
        | _ -> failwith "Continuation not found in call args")
      | _ -> failwith "Calling non-procedure")

let f e = eval' (Builtin.load Env.empty) (Tocps.f e)
