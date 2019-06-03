open Letreclang

let _ =
  Lexing.from_channel stdin
  |> Parser.f Lexer.f
  |> Eval.f
  |> Valenv.Val.to_str
  |> print_endline
