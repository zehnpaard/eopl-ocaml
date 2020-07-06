let rec eval' env = function
  | Cexp.Const n -> Val.Num n
  | Cexp.Var s -> (match Env.find env s with
      | Some v -> v
      | None -> failwith "Variable not found in environment")
  | Cexp.If (e1, e2, e3) -> (match eval' env e1 with
      | Val.Bool b -> eval' env (if b then e2 else e3)
      | _ -> failwith "Using non-boolean if-condition")
  | Cexp.Proc (ss, e) -> Val.Proc (ss, e, env)
  | Cexp.Call (e1, es) -> (match eval' env e1 with
      | Val.Proc (ss, e, env') ->
          let vs = List.map (eval' env) es in
          let env'' = Env.extend_list env' ss vs in
          eval' env'' e
      | Val.Op (_, f) ->
        let args = List.map (eval' env) es in
        (match List.rev args with
        | (Val.Proc([s],e,env'))::es' ->
          let v = f (List.rev es') in
          let env'' = Env.extend env' s v in
          eval' env'' e
        | _ -> failwith "")
      | _ -> failwith "Calling non-procedure")
  | Cexp.Cont (s,e) -> Val.Proc ([s], e, env)
  | Cexp.ApplyCont(k,e) -> (match eval' env k with
      | Val.Proc([s],e',env') ->
          let v = eval' env e in
          eval' (Env.extend env' s v) e'
      | _ -> failwith "ApplyCont on non-cont")

let f = eval' (Builtin.load Env.empty)
