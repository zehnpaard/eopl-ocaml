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
;;

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
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 - n2)
  | ZeroExp e ->
          valueOf e env (ZeroCont cont)
  | IfExp (e1, e2, e3) ->
          valueOf e1 env (IfCont e2 e3 env cont)
  | VarExp var ->
          applyCont cont (applyEnv env var)
  | LetExp (var, e, body) ->
          valueOf e env (LetCont var body env cont)
  | ProcExp (var, body) ->
          applyCont cont (ProcVal (Procedure (var, body, env)))
  | CallExp (func, arg) ->
          let p = expValToProc (valueOf func env) in
          applyProcedure p (valueOf arg env)
  | LetRecExp (fname, farg, fbody, body) ->
          valueOf body (ExtendEnvRec (fname, farg, fbody, env)) cont
and applyProcedure p v = match p with
  | Procedure (var, body, senv) -> valueOf body (ExtendEnv (var, v, senv))
and applyCont cont val1 = match cont with
  | EndCont -> val1
  | ZeroCont sc ->
          let val2 = BoolVal (0 = expValToNum val1) in
          applyCont sc val2
  | LetCont var1 body env sc ->
          valueOf body (ExtendEnv var1 val1 env) sc
  | IfCont e2 e3 env sc ->
          if expValToBool val1
          then valueOf e2 env sc
          else valueOf e3 env sc
;;

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
