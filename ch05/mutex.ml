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
  | SpawnExp of expression
;;

type environment =
  | EmptyEnv
  | ExtendEnv of symbol * expVal * environment
and expVal =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of procedure
  | RefVal of int
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
  | AssignCont of symbol * environment * continuation
  | EndMainThreadCont
  | EndSubThreadCont
  | SpawnCont of continuation
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


let readyQueue = Queue.create ();;
let maxTimeSlice = ref 0;;
let timeRemaining = ref 0;;
let finalAnswer = ref (NumVal 0);;

let initializeScheduler ticks =
    begin
        Queue.clear readyQueue;
        maxTimeSlice := ticks;
        timeRemaining := !maxTimeSlice;
        finalAnswer := NumVal 0;
    end
;;

let placeOnReadyQueue thread1 =
    Queue.add thread1 readyQueue
;;

let runNextThread () =
    if Queue.is_empty readyQueue then !finalAnswer
    else
        begin
            timeRemaining := !maxTimeSlice;
            (Queue.take readyQueue) ()
        end
;;

let decrementTimer () =
    timeRemaining := !timeRemaining - 1
;;

let isTimeExpired () =
    0 = !timeRemaining
;;

let setFinalAnswer v =
    finalAnswer := v
;;


type mutex = Mutex of bool ref * (unit -> expVal) Queue.t;;

let newMutex () = Mutex (ref false, Queue.create ());;

let waitForMutex m th = match m with
  | Mutex (isClosed, waitingThreads) ->
          if !isClosed
          then (Queue.add th waitingThreads; runNextThread ())
          else (isClosed := true; th ())
;;

let signalMutex m th = match m with
  | Mutex (isClosed, waitingThreads) ->
          begin
              if !isClosed
              then (isClosed := false; placeOnReadyQueue (Queue.take waitingThreads))
              else ();
              th ()
          end
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

exception CannotConvertNonRefVal;;
let expValToRef = function
  | RefVal n -> n
  | _ -> raise CannotConvertNonRefVal
;;


exception VariableNotFound;;
let rec applyEnv env var = match env with
  | EmptyEnv -> raise VariableNotFound
  | ExtendEnv (var1, val1, env1) ->
          if var = var1 then val1
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
          applyCont cont (deref (expValToRef (applyEnv env var)))
  | LetExp (var, e, body) ->
          valueOf e env (LetCont (var, body, env, cont))
  | ProcExp (var, body) ->
          applyCont cont (ProcVal (Procedure (var, body, env)))
  | CallExp (func, arg) ->
          valueOf func env (CallFuncCont (arg, env, cont))
  | AssignExp (var, exp1) ->
          valueOf exp1 env (AssignCont (var, env, cont))
  | SpawnExp e ->
          valueOf e env (SpawnCont cont)

and applyProcedure p v cont = match p with
  | Procedure (var, body, senv) ->
          let rv = RefVal (newRef v) in
          valueOf body (ExtendEnv (var, rv, senv)) cont

and applyCont cont val1 =
    if isTimeExpired ()
    then
        let currentThread = fun () -> applyCont cont val1 in
        (placeOnReadyQueue currentThread; runNextThread ())
    else
        (decrementTimer (); applyCont' cont val1)
and applyCont' cont val1 = match cont with
  | EndCont -> val1
  | ZeroCont sc ->
          let val2 = BoolVal (0 = expValToNum val1) in
          applyCont sc val2
  | LetCont (var1, body, env, sc) ->
          let rv = RefVal (newRef val1) in
          valueOf body (ExtendEnv (var1, rv, env)) sc
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
  | AssignCont (var, env, sc) ->
          begin
              setRef (expValToRef (applyEnv env var)) val1;
              applyCont sc val1
          end
  | EndMainThreadCont ->
          (setFinalAnswer val1; runNextThread ())
  | EndSubThreadCont ->
          runNextThread ()
  | SpawnCont sc ->
          let newThread =
              fun () -> applyProcedure (expValToProc val1) (NumVal 0) EndSubThreadCont
          in
          (placeOnReadyQueue newThread; applyCont sc (NumVal 0))
;;

let valueOfProgram = function Program e ->
    begin
        initializeStore ();
        initializeScheduler 10;
        valueOf e EmptyEnv EndMainThreadCont
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
