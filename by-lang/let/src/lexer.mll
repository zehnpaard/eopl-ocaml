{
open Parser
}

whitespace = [' ' '\t' '\n']
digit = ['0'-'9']
char = ['a'-'z' 'A'-'Z']
number = ('0'|['1'-'9'] digit*)
variable = char (char|digit)*

rule f = parse
  | whitespace* { f lexbuf }
  | number as n { Int n }
  | "zero?" { ZERO }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "-" { DIFF }
  | "," { COMMA }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQ }
  | "in" { IN }
  | variable as s { VAR s }
  | eof { EOF }
