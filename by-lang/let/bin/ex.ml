open Letlang

let _ =
  Lexing.from_channel stdin
  |> Parser.f Lexer.f
  |> ignore
