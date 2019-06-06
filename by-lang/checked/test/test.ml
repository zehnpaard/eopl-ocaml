open Checkedlang

let rep_str s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Eval.f
  |> Val.to_str
  |> print_endline

let _ =
  rep_str "-(5, 1)";
  rep_str "let x = 5 in -(x, 3)";
  rep_str "let z = 5 in let x = 3 in let y = -(x,1) in let x = 4 in -(z, -(x, y))";
  rep_str "let x = 7 in let y = 2 in let y = let x = -(x,1) in -(x,y) in -(-(x,8), y)";
  rep_str "let x = 5 in let y = if zero?(x) then 1 else 2 in -(x, y)";
  rep_str "let f = proc (x) -(x, 11) in (f (f 77))";
  rep_str "(proc (f) (f (f 77)) proc (x) -(x, 11))";
  rep_str "let x = 200 in let f = proc (z) -(z, x) in let x = 100 in let g  = proc (z) -(z, x) in -((f 1), (g 1))";
  rep_str "letrec zero (x) = if zero?(x) then x else (zero -(x, 1)) in (zero 5)";
