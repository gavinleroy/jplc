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

let%expect_test "simple-stmt-3" =
  Ppp.ppp_ast
    "let x = !10.;";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: expected one of the following types: (BoolType) but got FloatType |}]

let%expect_test "simple-stmt-4" =
  Ppp.ppp_ast
    "let x = 10. + 5;";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: expected type FloatType but got IntType |}]

let%expect_test "simple-stmt-5" =
  Ppp.ppp_ast
    "show 10. % false;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected type FloatType but got BoolType |}]

let%expect_test "simple-stmt-6" =
  Ppp.ppp_ast
    "show true >= false;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected one of the following types: (IntType FloatType) but got BoolType |}]

let%expect_test "simple-stmt-7" =
  Ppp.ppp_ast
    "show 1 || 10;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected type BoolType but got IntType |}]

let%expect_test "simple-stmt-8" =
  Ppp.ppp_ast
    "show true && 10;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-9" =
  Ppp.ppp_ast
    "let x = false;
     let y = if true then
     100 else true && x;";
  [%expect
    {|
      line: 2 column: 14
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-10" =
  Ppp.ppp_ast
    "let y = [false, true, 10];";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: array types must all be equal |}]

let%expect_test "simple-stmt-11" =
  Ppp.ppp_ast
    "let y = [11, 10, 10 == 12];";
  [%expect
    {|
      line: 1 column: 9
      	~~ type: array types must all be equal |}]

let%expect_test "simple-stmt-12" =
  Ppp.ppp_ast
    "
    fn foo(x : int, y : float) : float
    {
      return x as float * y;
    } let y = [foo(10, 3.5), 100];";
  [%expect
    {|
      line: 5 column: 15
      	~~ type: array types must all be equal |}]

let%expect_test "simple-stmt-13" =
  Ppp.ppp_ast
    "
    fn foo(x : int, y : float) : float
    {
      return x as float * y;
    } let y = foo(10, 3);";
  [%expect
    {|
      line: 5 column: 15
      	~~ type: expected parameter types of (IntType FloatType) but got (IntType IntType) |}]

let%expect_test "simple-stmt-14" =
  Ppp.ppp_ast
    "
    fn foo(x : int, y : float) : float
    {
      return x as float * y;
    } let y = foo(10, 3.);
    show y * 5;";
  [%expect
    {|
      line: 6 column: 10
      	~~ type: expected type FloatType but got IntType |}]
