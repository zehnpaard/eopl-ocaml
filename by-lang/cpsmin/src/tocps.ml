open Exp

let gensym =
  let n = ref (-1) in
  let f () = incr n; "gensym" ^ (string_of_int !n) in
  f 

let is_simple = function
| Const _ | Var _ -> true
| _ -> false

let rec g k e = match e with
| Const _ | Var _ -> Call(k,[e])
| Call(proc,es) ->
  let es = proc::es in
  let es' = List.filter (fun e -> not @@ is_simple e) es in
  let vs' = List.map (fun _ -> gensym ()) es' in
  let evs = List.rev @@ List.combine es' vs' in
  let f e = match List.assoc_opt e evs with
  | None -> e
  | Some v -> Var(v)
  in
  let es = List.map f es in
  let cont_body = Call(List.hd es,List.tl es @ [k]) in
  let g' cont_body (e, v) = g (Proc([v],cont_body)) e in
  List.fold_left g' cont_body evs
| If(cond,yes,no) ->
  let v = gensym () in
  let cont = Proc([v],If(Var(v),g k yes,g k no)) in
  g cont cond
| Proc(ss,body) ->
  let v = gensym () in
  Call(k,[Proc(ss@[v], g (Var v) body)])

let f e =
  let v = gensym () in
  g (Proc([v],Var v)) e