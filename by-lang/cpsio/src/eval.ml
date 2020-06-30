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
  | Exp.Let (ses, e2) ->
      let f env (s, e) = Env.extend env s (eval' env e) in
      let env = List.fold_left f env ses in
      eval' env e2
  | Exp.Proc (ss, e) -> Val.Proc (ss, e, env)
  | Exp.Call (e1, es) -> (match eval' env e1 with
      | Val.Proc (ss, e, env') ->
          let vs = List.map (eval' env) es in
          let env'' = Env.extend_list env' ss vs in
          eval' env'' e
      | _ -> failwith "Calling non-procedure")
  | Exp.LetRec (fname, arg, body, e) ->
      eval' (Env.extend_rec env fname arg body) e

let f = eval' Env.empty
