open Checkedlang

let check_eval exp =
  ignore (Typecheck.f exp); Eval.f exp

let f s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> check_eval
  |> Val.to_str
  |> print_endline

let rec rep acc =
  let s = read_line () in
  if s = "" then f acc
  else rep (acc ^ "\n" ^ s)

let _ = (print_string ">>> "; rep "")
