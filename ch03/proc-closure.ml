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
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
and expVal =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of procedure
and procedure =
  | Procedure of symbol * expression * environment
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
          applyEnv env var
  | LetExp (var, e, body) ->
          valueOf body (ExtendEnv (var, (valueOf e env), env))
  | ProcExp (var, body) ->
          ProcVal (Procedure (var, body, env))
  | CallExp (func, arg) ->
          let p = expValToProc (valueOf func env) in
          applyProcedure p (valueOf arg env)
and applyProcedure p v = match p with
  | Procedure (var, body, senv) -> valueOf body (ExtendEnv (var, v, senv))
;;

let valueOfProgram = function
  | Program e -> valueOf e EmptyEnv
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
