open Checkedlang

let _ =
  Lexing.from_channel stdin
  |> Parser.f Lexer.f
  |> Eval.f
  |> Val.to_str
  |> print_endline
