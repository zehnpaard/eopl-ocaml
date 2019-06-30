let rec check tenv = function
  | Exp.Const _ -> Type.Int
  | Exp.Var s -> (match Tenv.find tenv s with
      | Some t -> t
      | None -> failwith "Variable not typed")
  | Exp.ZeroP e -> 
      let t = check tenv e in
      begin
        Unify.f t Type.Int;
        Type.Bool
      end
  | Exp.Diff (e1, e2) ->
      begin
        Unify.f (check tenv e1) Type.Int;
        Unify.f (check tenv e2) Type.Int;
        Type.Int
      end
  | Exp.If (e1, e2, e3) ->
      begin
        Unify.f (check tenv e1) Type.Bool;
        Unify.f (check tenv e2) (check tenv e3);
        check tenv e2
      end
  | Exp.Let (s1, e1, e2) ->
      let t1 = check tenv e1 in
      let tenv' = Tenv.extend tenv s1 t1 in
      check tenv' e2
  | Exp.Proc (s, t, e) ->
      let t1 = Type.make_concrete t in
      let t2 = check (Tenv.extend tenv s t1) e in
      Type.Proc (t1, t2)
  | Exp.Call (e1, e2) ->
      let rt = Type.new_var () in
      let t1 = check tenv e1 in
      let t2 = check tenv e2 in
      begin
        Unify.f t1 (Type.Proc (t2, rt));
        rt
      end
  | Exp.LetRec (t1, fname, arg, t2, body, e) ->
      let rt = Type.make_concrete t1 in
      let at = Type.make_concrete t2 in
      let tenv' = Tenv.extend tenv fname (Type.Proc (at, rt)) in
      let tenv'' = Tenv.extend tenv' arg at in
      let bt = check tenv'' body in
      begin
        Unify.f bt rt;
        check tenv' e
      end

let f = check Tenv.empty
