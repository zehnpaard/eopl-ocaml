type symbol = Symbol of string;;

type expVal =
  | NumVal of int
  | BoolVal of bool
;;

Exception CannotConvertNonNumVal;;
let expValToNum = function
  | NumVal v -> v
  | _ -> raise CannotConvertNonNumval
;;

Exception CannotConvertNonBoolVal;;
let expValToBool = function
  | BoolVal b -> b
  | _ -> raise CannotConvertNonBoolVal
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
;;

Exception VariableNotFound;;

let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
          else applyEnv env1 var
;;
