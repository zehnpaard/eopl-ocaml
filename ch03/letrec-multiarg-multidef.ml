type symbol = Symbol of string;;

type expression =
  | ConstExp of int
  | DiffExp of expression * expression
  | ZeroExp of expression
  | IfExp of expression * expression * expression
  | VarExp of symbol
  | LetExp of symbol * expression * expression
  | ProcExp of symbol list * expression
  | CallExp of expression * expression list
  | LetRecExp of symbol list * (symbol list * expression) list * expression
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
  | ExtendEnvRec of symbol list * (symbol list * expression) list * environment
  | ExtendEnvMulti of symbol list * expVal list * environment
and expVal =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of procedure
and procedure =
  | Procedure of symbol list * expression * environment
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
exception VarValCountMismatch;;
let rec searchVars vars vals var = match vars, vals with
  | [], [] -> None
  | [], _ | _, [] -> raise VarValCountMismatch
  | var'::vars', val'::vals' -> if var' = var then Some val'
                                else searchVars vars' vals' var
;;
let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
          else applyEnv env1 var
  | ExtendEnvRec (vars, vals, env1) ->
          (match searchVars vars vals var with
          | None -> applyEnv env1 var
          | Some (args, body) -> ProcVal (Procedure (args, body, env)))
  | ExtendEnvMulti (vars, vals, env1) ->
          (match searchVars vars vals var with
          | None -> applyEnv env1 var
          | Some v -> v)
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
  | ProcExp (vars, body) ->
          ProcVal (Procedure (vars, body, env))
  | CallExp (func, args) ->
          let p = expValToProc (valueOf func env) in
          let vals = List.map (fun x -> valueOf x env) args in
          applyProcedure p vals
  | LetRecExp (fnames, fargbodies, body) ->
          valueOf body (ExtendEnvRec (fnames, fargbodies, env))
and applyProcedure p vals = match p with
  | Procedure (vars, body, senv) -> valueOf body (ExtendEnvMulti (vars, vals, senv))
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
