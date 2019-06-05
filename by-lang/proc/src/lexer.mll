{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit = ['0'-'9']
let char = ['a'-'z' 'A'-'Z']
let number = ('0'|['1'-'9'] digit*)
let variable = char (char|digit)*

rule f = parse
  | whitespace* { f lexbuf }
  | number as n { INT (int_of_string n) }
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
  | "proc" { PROC }
  | variable as s { VAR s }
  | eof { EOF }
