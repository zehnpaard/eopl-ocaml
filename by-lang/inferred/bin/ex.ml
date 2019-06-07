open Inferredlang

let check_eval exp =
  ignore (Typecheck.f exp); Eval.f exp

let _ =
  Lexing.from_channel stdin
  |> Parser.f Lexer.f
  |> check_eval
  |> Val.to_str
  |> print_endline
