type symbol = Symbol of string;;

type expression =
  | ConstExp of int
  | DiffExp of expression * expression
  | ZeroExp of expression
  | IfExp of expression * expression * expression
  | VarExp of symbol
  | LetExp of symbol * expression * expression
  | ProcExp of symbol * expression
  | CallExp of expression * expression
  | AssignExp of symbol * expression
  | NewPairExp of expression * expression
  | LeftOfPairExp of expression
  | RightOfPairExp of expression
  | SetLeftOfPairExp of expression * expression
  | SetRightOfPairExp of expression * expression
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
and expVal =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of procedure
  | RefVal of int
  | MutablePairVal of mutablePair
  | ThunkVal of thunk
and procedure =
  | Procedure of symbol * expression * environment
and mutablePair =
  | MutablePair of int * int
and thunk =
  | Thunk of expression * environment
;;

type store =
  | EmptyStore
  | AppendStore of int * expVal * store
;;

let the_store = ref EmptyStore;;

let getStore () = the_store;;
let initializeStore () = the_store := EmptyStore;;
let newRef v =
    let s = getStore () in
    let r = !s in
    let n = match r with
              | EmptyStore -> 0
              | AppendStore (n, _, _) -> n
    in
    let newr = AppendStore (n + 1, v, r) in
    (s := newr; n + 1)
;;

exception StoreNotFound;;
let rec applyStore s ref = match s with
  | EmptyStore  -> raise StoreNotFound
  | AppendStore (n, v, store1) ->
          if n = ref then v
          else applyStore store1 ref
;;
let deref ref = applyStore !(getStore ()) ref;;

let rec modifyStore s ref nv = match s with
  | EmptyStore  -> raise StoreNotFound
  | AppendStore (n, v, store1) ->
          if n = ref then AppendStore (n, nv, store1)
          else AppendStore (n, v, modifyStore store1 ref nv)
;;
let setRef ref v =
    let s = getStore () in
    s := modifyStore !s ref v
;;


let makePair v1 v2 = MutablePair (newRef v1, newRef v2);;
let leftOfPair = function MutablePair (l, _) -> deref l;;
let rightOfPair = function MutablePair (_, r) -> deref r;;
let setLeftOfPair p v = match p with
  | MutablePair (l, _) -> setRef l v;;
let setRightOfPair p v = match p with
  | MutablePair (_, r) -> setRef r v;;

type program = Program of expression;;

exception CannotConvertNonNumVal;;
let expValToNum = function
  | NumVal v -> v
  | _ -> raise CannotConvertNonNumVal
;;

exception CannotConvertNonBoolVal;;
let expValToBool = function
  | BoolVal b -> b
  | _ -> raise CannotConvertNonBoolVal
;;

exception CannotConvertNonProcVal;;
let expValToProc = function
  | ProcVal p -> p
  | _ -> raise CannotConvertNonProcVal
;;

exception CannotConvertNonRefVal;;
let expValToRef = function
  | RefVal n -> n
  | _ -> raise CannotConvertNonRefVal
;;

exception CannotConvertNonMutablePairVal;;
let expValToMutablePair = function
  | MutablePairVal p -> p
  | _ -> raise CannotConvertNonMutablePairVal
;;

exception CannotConvertNonThunkVal;;
let expValToThunk = function
  | ThunkVal n -> n
  | _ -> raise CannotConvertNonThunkVal
;;


exception VariableNotFound;;
let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
          else applyEnv env1 var
;;


let rec valueOf exp env = match exp with
  | ConstExp n ->
          NumVal n
  | DiffExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 - n2)
  | ZeroExp e ->
          BoolVal (0 = expValToNum (valueOf e env))
  | IfExp (e1, e2, e3) ->
          if expValToBool (valueOf e1 env)
          then valueOf e2 env
          else valueOf e3 env
  | VarExp var ->
          let r = expValToRef (applyEnv env var) in
          let v = deref r in
          (match v with
             | ThunkVal (Thunk (exp1, env1)) ->
                     let nv = valueOf exp1 env1 in
                     (setRef r nv; nv)
             | _ -> v)
  | LetExp (var, e, body) ->
          let rv = RefVal (newRef (valueOf e env)) in
          valueOf body (ExtendEnv (var, rv, env))
  | ProcExp (var, body) ->
          ProcVal (Procedure (var, body, env))
  | CallExp (func, arg) ->
          let p = expValToProc (valueOf func env) in
          let v = match arg with
                    | VarExp var -> applyEnv env var
                    | _ -> RefVal (newRef (ThunkVal (Thunk (arg, env))))
          in
          applyProcedure p v
  | AssignExp (var, exp1) ->
          let val1 = valueOf exp1 env in
          (setRef (expValToRef (applyEnv env var)) val1; val1)
  | NewPairExp (e1, e2) ->
          let v1 = valueOf e1 env in
          let v2 = valueOf e2 env in
          MutablePairVal (makePair v1 v2)
  | LeftOfPairExp e ->
          leftOfPair (expValToMutablePair (valueOf e env))
  | RightOfPairExp e ->
          rightOfPair (expValToMutablePair (valueOf e env))
  | SetLeftOfPairExp (pe, ve) ->
          let pv = valueOf pe env in
          let vv = valueOf ve env in
          (setLeftOfPair (expValToMutablePair pv) vv; vv)
  | SetRightOfPairExp (pe, ve) ->
          let pv = valueOf pe env in
          let vv = valueOf ve env in
          (setRightOfPair (expValToMutablePair pv) vv; vv)

and applyProcedure p v = match p with
  | Procedure (var, body, senv) ->
          valueOf body (ExtendEnv (var, v, senv))
;;

let valueOfProgram = function Program e ->
    begin
        initializeStore ();
        valueOf e EmptyEnv
    end
;;


let exp1 = DiffExp (ConstExp 5, ConstExp 3);;
let pgm1 = Program exp1;;
print_int (expValToNum (valueOfProgram pgm1));;

let exp2 = LetExp (Symbol "x", ConstExp 5, DiffExp (VarExp (Symbol "x"), ConstExp 2));;
let pgm2 = Program exp2;;
print_int (expValToNum (valueOfProgram pgm2));;

let exp3 = IfExp (ZeroExp (DiffExp (ConstExp 5, ConstExp 5)), exp1, exp2);;
let pgm3 = Program exp3;;
print_int (expValToNum (valueOfProgram pgm3));;
