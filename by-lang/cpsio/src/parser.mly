%token <int> INT
%token <string> VAR
%token ZERO
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token IF
%token LET
%token PROC
%token LETREC
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
  | n = INT { Exp.Const n }
  | s = VAR { Exp.Var s }
  | LPAREN; ZERO; e = expr; RPAREN { Exp.ZeroP e }
  | LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If (e1, e2, e3) }
  | LPAREN; LET; LBRACK; s = VAR; e1 = expr; RBRACK; e2 = expr; RPAREN { Exp.Let ([(s, e1)], e2) }
  | LPAREN; LET; LBRACK; ses = nonempty_list(varexp); RBRACK; e2 = expr; RPAREN { Exp.Let (ses, e2) }
  | LPAREN PROC; LBRACK; ss = list(VAR); RBRACK; e = expr; RPAREN { Exp.Proc (ss, e) }
  | LPAREN; e1 = expr; es = list(expr); RPAREN { Exp.Call (e1, es) }
  | LPAREN; LETREC; LBRACK; fname = VAR; LBRACK; args = list(VAR); RBRACK;
     body = expr; RBRACK; e = expr; RPAREN { Exp.LetRec ([(fname, args, body)], e) }
  | LPAREN; LETREC; LBRACK; fns = list(recexp); RBRACK;
     e = expr; RPAREN { Exp.LetRec (fns, e) }

varexp :
  | LPAREN; s = VAR; e = expr; RPAREN { (s, e) }
recexp :
  | LPAREN; fname = VAR; LBRACK; args = list(VAR); RBRACK; body = expr; RPAREN { (fname, args, body) }