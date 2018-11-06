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
  | LetRecExp of symbol * symbol * expression * expression
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
  | ExtendEnvRec of symbol * symbol * expression * environment
and expVal =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of procedure
and procedure =
  | Procedure of symbol * expression * environment
;;

type continuation =
  | EndCont
  | ZeroCont of continuation
  | LetCont of symbol * expression * environment * continuation
  | IfCont of expression * expression * environment * continuation
  | Diff1Cont of expression * environment * continuation
  | Diff2Cont of expVal * continuation
  | CallFuncCont of expression * environment * continuation
  | CallArgCont of expVal * continuation
;;

type program = Program of expression;;

let exp = ref (ConstExp 0);;
let env = ref EmptyEnv;;
let cont = ref EndCont;;
let val1 = ref (NumVal 0);;
let proc1 = ref (Procedure (Symbol "x", ConstExp 0, EmptyEnv));;


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


exception VariableNotFound;;
let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
          else applyEnv env1 var
  | ExtendEnvRec (fname, arg, body, env1) ->
          if var = fname then ProcVal (Procedure (arg, body, env))
          else applyEnv env1 var
;;

let rec valueOf exp env cont = match exp with
  | ConstExp n ->
          applyCont cont (NumVal n)
  | DiffExp (e1, e2) ->
          valueOf e1 env (Diff1Cont (e2, env, cont))
  | ZeroExp e ->
          valueOf e env (ZeroCont cont)
  | IfExp (e1, e2, e3) ->
          valueOf e1 env (IfCont (e2, e3, env, cont))
  | VarExp var ->
          applyCont cont (applyEnv env var)
  | LetExp (var, e, body) ->
          valueOf e env (LetCont (var, body, env, cont))
  | ProcExp (var, body) ->
          applyCont cont (ProcVal (Procedure (var, body, env)))
  | CallExp (func, arg) ->
          valueOf func env (CallFuncCont (arg, env, cont))
  | LetRecExp (fname, farg, fbody, body) ->
          valueOf body (ExtendEnvRec (fname, farg, fbody, env)) cont
and applyProcedure p v cont = match p with
  | Procedure (var, body, senv) ->
          valueOf body (ExtendEnv (var, v, senv)) cont
and applyCont cont val1 = match cont with
  | EndCont -> val1
  | ZeroCont sc ->
          let val2 = BoolVal (0 = expValToNum val1) in
          applyCont sc val2
  | LetCont (var1, body, env, sc) ->
          valueOf body (ExtendEnv (var1, val1, env)) sc
  | IfCont (e2, e3, env, sc) ->
          if expValToBool val1
          then valueOf e2 env sc
          else valueOf e3 env sc
  | Diff1Cont (e2, env, sc) ->
          valueOf e2 env (Diff2Cont (val1, sc))
  | Diff2Cont (v1, sc) ->
          applyCont sc (NumVal (expValToNum v1 - expValToNum val1))
  | CallFuncCont (arg, env, sc) ->
          valueOf arg env (CallArgCont (val1, sc))
  | CallArgCont (v1, sc) ->
          applyProcedure (expValToProc v1) val1 sc
;;

let valueOfProgram = function
  | Program e -> valueOf e EmptyEnv EndCont
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
