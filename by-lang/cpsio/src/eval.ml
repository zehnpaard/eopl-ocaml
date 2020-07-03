let rec eval' env = function
  | Exp.Const n -> Val.Num n
  | Exp.Var s -> (match Env.find env s with
      | Some v -> v
      | None -> failwith "Variable not found in environment")
  | Exp.ZeroP e -> (match eval' env e with
      | Val.Num n -> Val.Bool (n = 0)
      | _ -> failwith "Zero-checking non-numeric value")
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
      | Val.Op (_, f) -> f (List.map (eval' env) es)
      | _ -> failwith "Calling non-procedure")
  | Exp.LetRec (fns, e) ->
      let rec split3 acc1 acc2 acc3= function
      | [] -> List.rev acc1, List.rev acc2, List.rev acc3
      | (a,b,c)::xs -> split3 (a::acc1) (b::acc2) (c::acc3) xs
      in
      let fnames, argss, bodys = split3 [] [] [] fns in
      eval' (Env.extend_rec_list env fnames argss bodys) e

let f = eval' (Builtin.load Env.empty)
