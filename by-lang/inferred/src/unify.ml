let rec f t1 t2 =
  match Subst.apply t1, Subst.apply t2 with
    | t1', t2' when t1' = t2' -> ()
    | Type.Var _ as t1', t2' ->
        if Type.occurs t1' t2' then failwith "No occurrence condition violated"
        else Subst.extend t1' t2'
    | t1', (Type.Var _ as t2') ->
        if Type.occurs t2' t1' then failwith "No occurrence condition violated"
        else Subst.extend t2' t1'
    | Type.Proc (atype1, rtype1), Type.Proc (atype2, rtype2) ->
        begin
          f atype1 atype2;
          f rtype1 rtype2
        end
    | _ -> failwith "Unification failed"
