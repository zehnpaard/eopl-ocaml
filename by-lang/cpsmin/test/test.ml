open Cpsmin

let rep_str s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Tocps.f
  |> Eval.f
  |> Val.to_str
  |> print_endline

let _ =
  rep_str "-";
  rep_str "(- 5 1)";
  rep_str "(+ 5 1 2)";
  rep_str "(proc [a b c] (+ a b c))";
  rep_str "((proc [a b c] (+ a b c)) 4 5 6)";
  rep_str "(if (zero? 0) 1 2)";
  rep_str "(if (zero? 3) 1 2)";
  rep_str "(if (zero? 0) (+ 1 2) 4)";
  rep_str "(if (zero? 3) 1 (+ 1 2))";