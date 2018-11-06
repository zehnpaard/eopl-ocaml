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

let rec valueOf () = match !exp with
  | ConstExp n ->
          begin
              val1 := NumVal n;
              applyCont ()
          end
  | DiffExp (e1, e2) ->
          begin
              cont := Diff1Cont (e2, !env, !cont);
              exp := e1;
              valueOf ()
          end
  | ZeroExp e ->
          begin
              cont := ZeroCont !cont;
              exp := e;
              valueOf ()
          end
  | IfExp (e1, e2, e3) ->
          begin
              cont := IfCont (e2, e3, !env, !cont);
              exp := e1;
              valueOf ()
          end
  | VarExp var ->
          begin
              val1 := applyEnv !env var;
              applyCont ()
          end
  | LetExp (var, e, body) ->
          begin
              cont := LetCont (var, body, !env, !cont);
              exp := e;
              valueOf ()
          end
  | ProcExp (var, body) ->
          begin
              val1 := ProcVal (Procedure (var, body, !env));
              applyCont ()
          end
  | CallExp (func, arg) ->
          begin
              cont := CallFuncCont (arg, !env, !cont);
              exp := func;
              valueOf ()
          end
  | LetRecExp (fname, farg, fbody, body) ->
          begin
              env := ExtendEnvRec (fname, farg, fbody, !env);
              exp := body;
              valueOf ()
          end
and applyProcedure () = match !proc1 with
  | Procedure (var, body, senv) ->
          begin
              env := ExtendEnv (var, !val1, senv);
              exp := body;
              valueOf ()
          end
and applyCont () = match !cont with
  | EndCont -> !val1
  | ZeroCont sc ->
          begin
              cont := sc;
              val1 := BoolVal (0 = expValToNum !val1);
              applyCont ()
          end
  | LetCont (var1, body, env, sc) ->
          begin
              cont := sc;
              env := ExtendEnv (var1, !val1, !env);
              exp := body;
              valueOf ()
          end
  | IfCont (e2, e3, env, sc) ->
          begin
              cont := sc;
              exp := if expValToBool !val1 then e2 else e3;
              valueOf ()
          end
  | Diff1Cont (e2, env, sc) ->
          begin
              cont := Diff2Cont (!val1, sc);
              exp := e2;
              valueOf ()
          end
  | Diff2Cont (v1, sc) ->
          begin
              cont := sc;
              val1 := NumVal (expValToNum v1 - expValToNum !val1);
              applyCont ()
          end
  | CallFuncCont (arg, env, sc) ->
          begin
              cont := CallArgCont (!val1, sc);
              exp := arg;
              valueOf ()
          end
  | CallArgCont (v1, sc) ->
          begin
              cont := sc;
              proc1 := expValToProc v1;
              applyProcedure ()
          end
;;

let valueOfProgram = function
  | Program e ->
          begin
              cont := EndCont;
              env := EmptyEnv;
              exp := e;
              valueOf ()
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
