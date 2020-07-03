open Cpsmin

let rep_str s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Eval.f
  |> Val.to_str
  |> print_endline

let _ =
  rep_str "(- 5 1)";
  rep_str "((proc [a b c] (+ a b c)) 4 5 6)";