type symbol = Symbol of string;;

type ttype =
  | TInt
  | TBool
  | TFunc of ttype * ttype
  | TVar of int
;;

type otype =
  | ONoType
  | OAType of ttype

type expression =
  | ConstExp of int
  | DiffExp of expression * expression
  | ZeroExp of expression
  | IfExp of expression * expression * expression
  | VarExp of symbol
  | LetExp of symbol * expression * expression
  | ProcExp of symbol * otype * expression
  | CallExp of expression * expression
  | LetRecExp of otype * symbol * symbol * otype * expression * expression
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
and typeEnvironment =
  | EmptyTenv
  | ExtendTenv of symbol * ttype * typeEnvironment
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

let rec applyTenv tenv var = match tenv with
  | EmptyTenv -> raise VariableNotFound
  | ExtendTenv (var1, ttype1, tenv1) ->
          if var = var1 then ttype1
          else applyTenv tenv1 var
;;

type subst = Subst of ttype list * ttype list;;

let rec applyOneSubst ty0 tvar ty1 = match ty0 with
  | TInt -> TInt
  | TBool -> TBool
  | TFunc (ty2, ty3) -> TFunc (applyOneSubst ty2 tvar ty1, applyOneSubst ty3 tvar ty1)
  | TVar n -> if ty0 = tvar then ty1 else ty0
;;

let extendSubst subst1 tvar ty0 = match subst1 with
  | Subst (tvars, tys) ->
          let tys' = List.map (fun x -> applyOneSubst x tvar ty0) tys in
          Subst (tvar::tvars, ty0::tys')

exception KeyValCountMismatch
let applyExactSubst subst ty =
    let rec f = function
        | [], [] -> ty
        | [], _ | _, [] -> raise KeyValCountMismatch
        | tvar::tvs, ty1::tys -> if tvar = ty then ty1 else f (tvs, tys)
    in
    match subst with Subst (tvars, tys) -> f (tvars, tys)
;;

let rec applySubstToType subst ty = match ty with
  | TInt -> TInt
  | TBool -> TBool
  | TFunc (ty1, ty2) -> TFunc (applySubstToType subst ty1, applySubstToType subst ty2)
  | TVar n -> applyExactSubst subst ty
;;

let rec noOccurrence tvar ty = match ty with
  | TInt -> true
  | TBool -> true
  | TFunc (ty1, ty2) -> noOccurrence tvar ty1 && noOccurrence tvar ty2
  | TVar n -> tvar != ty
;;

exception NoOccurrenceViolation;;
exception UnificationFailure;;
let rec unifier ty1 ty2 subst =
    let ty1' = applySubstToType subst ty1 in
    let ty2' = applySubstToType subst ty2 in
    match ty1', ty2' with
      | _, _ when ty1' = ty2' -> subst
      | TVar n, _ ->
              if noOccurrence ty1' ty2'
              then extendSubst subst ty1' ty2'
              else raise NoOccurrenceViolation
      | _, TVar n ->
              if noOccurrence ty2' ty1'
              then extendSubst subst ty2' ty1'
              else raise NoOccurrenceViolation
      | TFunc (ta1, tr1), TFunc (ta2, tr2) ->
              unifier tr1 tr2 (unifier ta1 ta2 subst)
      | _, _ -> raise UnificationFailure
;;

let getFreshTVar =
    let x = ref (-1) in
    fun () -> (x := !x+1; TVar !x)
;;

let otypeTottype = function
  | ONoType -> getFreshTVar ()
  | OAType t -> t
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
  | ProcExp (var, vtype, body) ->
          ProcVal (Procedure (var, body, env))
  | CallExp (func, arg) ->
          let p = expValToProc (valueOf func env) in
          applyProcedure p (valueOf arg env)
  | LetRecExp (ftype, fname, farg, atype, fbody, body) ->
          valueOf body (ExtendEnvRec (fname, farg, fbody, env))
and applyProcedure p v = match p with
  | Procedure (var, body, senv) -> valueOf body (ExtendEnv (var, v, senv))
;;

let valueOfProgram = function
  | Program e -> valueOf e EmptyEnv
;;

type typeResult = TypeResult of ttype * subst;;

exception TypeError;;
let rec typeOf exp tenv subst = match exp with
  | ConstExp n ->
          TypeResult (TInt, subst)
  | ZeroExp e ->
          let TypeResult (ty, subst') = typeOf e tenv subst in
          TypeResult (TBool, unifier ty TInt subst' e)
  | DiffExp (e1, e2) ->
          let TypeResult (ty1, subst1) = typeOf e1 tenv subst in
          let subst1' = unifier ty1 TInt subst1 e1
          let TypeResult (ty2, subst2) = typeOf e2 tenv subst1' in
          let subst2' = unifier ty2 TInt subst2 e2
          TypeResult (TInt, subst2')
  | IfExp (e1, e2, e3) ->
  | VarExp var ->
  | LetExp (var, e, body) ->
  | ProcExp (var, vtype, body) ->
  | CallExp (func, arg) ->
  | LetRecExp (rtype, fname, farg, atype, fbody, body) ->
;;

let typeOfProgram = function
  | Program e ->
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
