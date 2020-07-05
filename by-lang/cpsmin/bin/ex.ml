open Cpsmin

let f s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Tocps.f
  |> Eval.f
  |> Val.to_str
  |> print_endline

let rec rep acc =
  let s = read_line () in
  if s = "" then f acc
  else rep (acc ^ "\n" ^ s)

let _ = (print_string ">>> "; rep "")
