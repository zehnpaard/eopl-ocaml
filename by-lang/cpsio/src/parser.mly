%token <int> INT
%token <string> VAR
%token ZERO
%token LPAREN
%token RPAREN
%token DIFF
%token COMMA
%token IF
%token THEN
%token ELSE
%token LET
%token EQ
%token IN
%token PROC
%token LETREC
%token EOF

%start <Exp.t> f

%%

f : e = expr; EOF { e }

expr :
  | n = INT { Exp.Const n }
  | s = VAR { Exp.Var s }
  | ZERO; LPAREN; e = expr; RPAREN { Exp.ZeroP e }
  | DIFF; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Exp.Diff (e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { Exp.If (e1, e2, e3) }
  | LET; s = VAR; EQ; e1 = expr; IN; e2 = expr { Exp.Let (s, e1, e2) }
  | PROC; LPAREN; s = VAR; RPAREN; e = expr { Exp.Proc (s, e) }
  | LPAREN; e1 = expr; e2 = expr; RPAREN { Exp.Call (e1, e2) }
  | LETREC; fname = VAR; LPAREN; arg = VAR; RPAREN;
     EQ; body = expr; IN; e = expr; { Exp.LetRec (fname, arg, body, e) }
