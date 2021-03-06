type symbol = Symbol of string;;

type expVal =
  | NumVal of int
  | BoolVal of bool
;;

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

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
;;

exception VariableNotFound;;

let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
          else applyEnv env1 var
;;


type expression =
  | ConstExp of int
  | MinusExp of expression
  | AddExp of expression * expression
  | DiffExp of expression * expression
  | MulExp of expression * expression
  | DivExp of expression * expression
  | ZeroExp of expression
  | EqualExp of expression * expression
  | GreaterExp of expression * expression
  | LessExp of expression * expression
  | IfExp of expression * expression * expression
  | VarExp of symbol
  | LetExp of symbol * expression * expression
;;

type program = Program of expression;;


let rec valueOf exp env = match exp with
  | ConstExp n ->
          NumVal n
  | MinusExp e ->
          NumVal (- (expValToNum (valueOf e env)))
  | AddExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 + n2)
  | DiffExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 - n2)
  | MulExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 * n2)
  | DivExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          NumVal (n1 / n2)
  | ZeroExp e ->
          BoolVal (0 = expValToNum (valueOf e env))
  | EqualExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          BoolVal (n1 = n2)
  | GreaterExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          BoolVal (n1 > n2)
  | LessExp (e1, e2) ->
          let n1 = expValToNum (valueOf e1 env) in
          let n2 = expValToNum (valueOf e2 env) in
          BoolVal (n1 < n2)
  | IfExp (e1, e2, e3) ->
          if expValToBool (valueOf e1 env)
          then valueOf e2 env
          else valueOf e3 env
  | VarExp var ->
          applyEnv env var
  | LetExp (var, e, body) ->
          valueOf body (ExtendEnv (var, (valueOf e env), env))
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

let exp4 = DiffExp (ConstExp 5, MinusExp (ConstExp 3));;
let pgm4 = Program exp4;;
print_int (expValToNum (valueOfProgram pgm4));;

let exp5 = DivExp (MulExp (ConstExp 6, AddExp (ConstExp 5, MinusExp (ConstExp 1))), ConstExp 2);;
let pgm5 = Program exp5;;
print_int (expValToNum (valueOfProgram pgm5));;
