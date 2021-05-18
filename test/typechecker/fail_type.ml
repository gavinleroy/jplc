(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "simple-cmd-1" =
  Ppp.ppp_ast
    "read image \"someimg.png\" to img[H, W];
     write image img to \"newimg.png\";";
  [%expect
    {|
      line: 1 column: 1
      	~~ type: cannot pattern match within 'read' command |}]

let%expect_test "simple-stmt-1" =
  Ppp.ppp_ast
    "assert 1, \"print me!\";";
  [%expect
    {|
      line: 1 column: 1
      	~~ type: expected type BoolType but got IntType |}]

let%expect_test "simple-stmt-2" =
  Ppp.ppp_ast
    "let x = -true;";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: expected one of the following types: (IntType FloatType) but got BoolType |}]

let%expect_test "simple-stmt-2" =
  Ppp.ppp_ast
    "let x = !10.;";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: expected type BoolType but got FloatType |}]
