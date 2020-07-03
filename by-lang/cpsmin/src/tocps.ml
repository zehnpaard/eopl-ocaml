open Exp

let gensym =
  let n = ref (-1) in
  let f () = incr n; "gensym" ^ (string_of_int !n) in
  f 

let rec g k e = match e with
| Const _ | Var _ -> Call(k,[e])
| Call(proc,es) ->
  let proc_var = gensym () in
  let vs = List.map (fun _ -> gensym ()) es in
  let cont_body = Call(Var(proc_var), (List.map (fun v -> Var(v)) vs) @ [k]) in
  let evs = List.rev @@ List.combine es vs in
  let g' cont_body (e, v) = g (Proc([v],cont_body)) e in
  let cont_body = List.fold_left g' cont_body evs in
  let c = Proc([proc_var],cont_body) in
  g c proc
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