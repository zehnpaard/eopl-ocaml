{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let number = ('0'|['1'-'9'] digit*)
let char = ['-' '+' '*' '/' '?' '!' '=']
let variable = (alpha|char) (alpha|char|digit)*

rule f = parse
  | whitespace* { f lexbuf }
  | number as n { INT (int_of_string n) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "if" { IF }
  | "let" { LET }
  | "proc" { PROC }
  | "letrec" { LETREC }
  | variable as s { VAR s }
  | eof { EOF }
