%token <int> INT
%token <string> VAR
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token IF
%token PROC
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
  | n = INT { Exp.Const n }
  | s = VAR { Exp.Var s }
  | LPAREN; IF; e1 = expr; e2 = expr; e3 = expr; RPAREN { Exp.If (e1, e2, e3) }
  | LPAREN PROC; LBRACK; ss = list(VAR); RBRACK; e = expr; RPAREN { Exp.Proc (ss, e) }
  | LPAREN; e1 = expr; es = list(expr); RPAREN { Exp.Call (e1, es) }