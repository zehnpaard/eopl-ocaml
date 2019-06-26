let rec check tenv subst = function
  | Exp.Const _ -> (Type.Int, subst)
  | Exp.Var s -> (match Tenv.find tenv s with
      | Some t -> (t, subst)
      | None -> failwith "Variable not typed")
  | Exp.ZeroP e -> 
      let t, subst1 = check tenv subst e in
      let subst2 = Unify.f t Type.Int subst1 e in
      (Type.Bool, subst2)
  | Exp.Diff (e1, e2) ->
      let t1, subst1 = check tenv subst e1 in
      let subst1' = Unify.f t1 Type.Int subst1 e1 in
      let t2, subst2 = check tenv subst1' e2 in
      let subst2' = Unify.f t2 Type.Int subst2 e2 in
      (Type.Int, subst2')
  | Exp.If (e1, e2, e3) as e ->
      let t1, subst1 = check tenv subst e1 in
      let subst1' = Unify.f t1 Type.Bool subst1 e1 in
      let t2, subst2 = check tenv subst1' e2 in
      let t3, subst3 = check tenv subst2 e3 in
      let subst' = Unify.f t2 t3 subst3 e in
      (t2, subst')
  | Exp.Let (s1, e1, e2) ->
      let t1, subst1 = check tenv subst e1 in
      check (Tenv.extend tenv s1 t1) subst1 e2
  | Exp.Proc (s, t, e) ->
      let t1 = Type.make_concrete t in
      let t2, subst2 = check (Tenv.extend tenv s t1) subst e in
      (Type.Proc (t1, t2), subst2)
  | Exp.Call (e1, e2) as e ->
      let rt = Type.new_var () in
      let t1, subst1 = check tenv subst e1 in
      let t2, subst2 = check tenv subst1 e2 in
      let subst' = Unify.f t1 (Type.Proc (t2, rt)) subst2 e in
      (rt, subst')
  | Exp.LetRec (t1, fname, arg, t2, body, e) ->
      let rt = Type.make_concrete t1 in
      let at = Type.make_concrete t2 in
      let tenv' = Tenv.extend tenv fname (Type.Proc (at, rt)) in
      let tenv'' = Tenv.extend tenv' arg at in
      let bt, subst' = check tenv'' subst body in
      let subst'' = Unify.f bt rt subst' body in
      check tenv' subst'' e

let f = check Tenv.empty Subst.empty
