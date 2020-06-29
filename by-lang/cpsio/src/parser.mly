%token <int> INT
%token <string> VAR
%token ZERO
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token DIFF
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
  | LPAREN; DIFF; e1 = expr; e2 = expr; RPAREN { Exp.Diff (e1, e2) }
  | LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If (e1, e2, e3) }
  | LPAREN; LET; LBRACK; s = VAR; e1 = expr; RBRACK; e2 = expr; RPAREN { Exp.Let (s, e1, e2) }
  | LPAREN PROC; s = VAR; e = expr; RPAREN { Exp.Proc (s, e) }
  | LPAREN; e1 = expr; e2 = expr; RPAREN { Exp.Call (e1, e2) }
  | LPAREN; LETREC; fname = VAR; arg = VAR;
     body = expr; e = expr; RPAREN { Exp.LetRec (fname, arg, body, e) }
