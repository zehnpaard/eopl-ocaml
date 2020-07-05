let parse s = Lexing.from_string s |> Parser.f Lexer.f
let cps s = parse s |> Tocps.f
let cpss s = cps s |> Cexp.to_string
let cps_eval s = cps s |> Eval.f
