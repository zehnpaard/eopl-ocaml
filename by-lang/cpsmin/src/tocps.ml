let gensym =
  let n = ref (-1) in
  let f () = incr n; "gensym" ^ (string_of_int !n) in
  f 

let is_simple = function
| Exp.Const _ | Exp.Var _ -> true
| _ -> false

let rec simple_transform = function
| Exp.Const n -> Cexp.Const n
| Exp.Var s -> Cexp.Var s
| Exp.Call(proc,es) -> Cexp.Call(simple_transform proc, List.map simple_transform es)
| Exp.If(cond,yes,no) -> Cexp.If(simple_transform cond, simple_transform yes, simple_transform no)
| Exp.Proc(ss,body) -> Cexp.Proc(ss, simple_transform body)

let rec g k e = match e with
| Exp.Const _ | Exp.Var _ -> Cexp.Call(k,[simple_transform e])
| Exp.Call(proc,es) ->
  let es = proc::es in
  let es' = List.filter (fun e -> not @@ is_simple e) es in
  let vs' = List.map (fun _ -> gensym ()) es' in
  let evs = List.rev @@ List.combine es' vs' in
  let f e = match List.assoc_opt e evs with
  | None -> simple_transform e
  | Some v -> Cexp.Var(v)
  in
  let es = List.map f es in
  let cont_body = Cexp.Call(List.hd es,List.tl es @ [k]) in
  let g' cont_body (e, v) = g (Cexp.Proc([v],cont_body)) e in
  List.fold_left g' cont_body evs
| Exp.If(cond,yes,no) ->
  let v = gensym () in
  let cont = Cexp.Proc([v],Cexp.If(Cexp.Var(v),g k yes,g k no)) in
  g cont cond
| Exp.Proc(ss,body) ->
  let v = gensym () in
  Cexp.Call(k,[Cexp.Proc(ss@[v], g (Cexp.Var v) body)])

let f e =
  let v = gensym () in
  g (Cexp.Proc([v],Cexp.Var v)) e