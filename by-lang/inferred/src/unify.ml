let rec f t1 t2 subst e =
  match Subst.apply subst t1, Subst.apply subst t2 with
    | t1', t2' when t1' = t2' -> subst
    | Type.Var _ as t1', t2' ->
        if Type.occurs t1' t2' then failwith "No occurrence condition violated"
        else Subst.extend subst t1' t2'
    | t1', (Type.Var _ as t2') ->
        if Type.occurs t2' t1' then failwith "No occurrence condition violated"
        else Subst.extend subst t2' t1'
    | Type.Proc (atype1, rtype1), Type.Proc (atype2, rtype2) ->
        let subst' = f atype1 atype2 subst e in
        f rtype1 rtype2 subst' e
    | _ -> failwith "Unification failed"
