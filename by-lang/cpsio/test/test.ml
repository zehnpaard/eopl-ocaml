open Letreclang

let rep_str s =
  Lexing.from_string s
  |> Parser.f Lexer.f
  |> Eval.f
  |> Val.to_str
  |> print_endline

let _ =
  rep_str "(- 5 1)";
  rep_str "(let [x 5] (- x 3))";
  rep_str "(let [z 5] (let [x 3] (let [y (- x 1)] (let [x 4] (- z (- x y))))))";
  rep_str "(let [x 7] (let [y 2] (let [y (let [x (- x 1)] (- x y))] (- (- x 8) y))))";
  rep_str "(let [x 5] (let [y (if (zero? x) 1 2)] (- x y)))";
  rep_str "(let [f (proc [x] (- x 11))] (f (f 77)))";
  rep_str "((proc [f] (f (f 77))) (proc [x] (- x 11)))";
  rep_str "(let [x 200] (let [f (proc [z] (- z x))] (let [x 100] (let [g (proc [z] (- z x))] (- (f 1) (g 1))))))";
  rep_str "(letrec [zero [x] (if (zero? x) x (zero (- x 1)))] (zero 5))";
  rep_str "(let [(z 5) (x 3) (y (- x 1)) (x 4)] (- z (- x y)))";
  rep_str "(let [(x 7) (y 2) (y (let [x (- x 1)] (- x y)))] (- (- x 8) y))";
  rep_str "(let [(x 5) (y (if (zero? x) 1 2))] (- x y))";
  rep_str "(let [(x 200) (f (proc [z] (- z x))) (x 100) (g (proc [z] (- z x)))] (- (f 1) (g 1)))";
  rep_str "((proc [x y] (- x y)) 5 3)";
  rep_str "(letrec [zeroand [x y] (if (zero? x) (- x y) (zeroand (- x 1) y))] (zeroand 5 3))";
  rep_str "(letrec [(even [x]
                      (if (zero? x)
                        (zero? 0)
                        (odd (- x 1))))
                    (odd [x]
                      (if (zero? x)
                        (zero? 1)
                        (even (- x 1))))]
              (odd 2))";