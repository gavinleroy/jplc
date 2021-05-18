(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "simple-cmd-1" =
  Ppp.ppp_ast
    "print \"Hallo, Welt!\"";
  [%expect
    {|
      line: 1 column: 21
      	~~ parser: unexpected symbol (perhaps a missing ';') |}]

let%expect_test "simple-stmt-1" =
  Ppp.ppp_ast
    "let x 5;";
  [%expect
    {|
      line: 1 column: 7
      	~~ parser: expected '=' |}]

let%expect_test "simple-stmt-2" =
  Ppp.ppp_ast
    "let x = 5";
  [%expect
    {|
      line: 1 column: 10
      	~~ parser: unexpected symbol (perhaps a missing ';') |}]

let%expect_test "simple-stmt-3" =
  Ppp.ppp_ast
    "let float = 5.";
  [%expect
    {|
      line: 1 column: 5
      	~~ parser: expected symbol |}]
