(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

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

let%expect_test "simple-stmt-15" =
  Ppp.ppp_ast
    "let { a    , b } = { 1, true, false };";
  [%expect
    {|
      line: 1 column: 5
      	~~ type: expected tuple of 2 elements but got 3 |}]

let%expect_test "simple-stmt-16" =
  Ppp.ppp_ast
    "let { a, b } = { 1 };";
  [%expect
    {|
      line: 1 column: 5
      	~~ type: expected tuple of 2 elements but got 1 |}]

let%expect_test "simple-stmt-17" =
  Ppp.ppp_ast
    "let { a, b, c } = { 1, 2, false };
     show a * b + c;";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-18" =
  Ppp.ppp_ast
    "show sum[i : 5.] 10;";
  [%expect
    {|
      line: 1 column: 14
      	~~ type: expected type IntType but got FloatType |}]

let%expect_test "simple-stmt-19" =
  Ppp.ppp_ast
    "show sum[i : 5] false;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected one of the following types: (IntType FloatType) but got BoolType |}]

let%expect_test "simple-stmt-20" =
  Ppp.ppp_ast
    "let z=false;
     show sum[a:0,b:1,c:2,d:z,e:4] 0;";
  [%expect
    {|
      line: 2 column: 29
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-22" =
  Ppp.ppp_ast
    "let z=false;
     show array[a:0,b:1,c:2,d:z,e:4] 0;";
  [%expect
    {|
      line: 2 column: 31
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-23" =
  Ppp.ppp_ast
    "show array[a:1,b:1,c:2,d:3.,e:4] 0;";
  [%expect
    {|
      line: 1 column: 26
      	~~ type: expected type IntType but got FloatType |}]

let%expect_test "simple-stmt-24" =
  Ppp.ppp_ast
    "show sum [a:1,b:1,c:2,d:3,e:4] 1 == 0;";
  [%expect
    {|
      line: 1 column: 6
      	~~ type: expected one of the following types: (IntType FloatType) but got BoolType |}]

let%expect_test "simple-stmt-25" =
  Ppp.ppp_ast
    "let d = { 1, 2, 3 };
     show d{ 3 };";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: index 3 out of tuple range |}]

let%expect_test "simple-stmt-26" =
  Ppp.ppp_ast
    "let d = { 1, 2, 3 };
     show d{ -1 }; // shouldn't parse!";
  [%expect
    {|
      line: 2 column: 14
      	~~ parser: unexpected command |}]

let%expect_test "simple-stmt-27" =
  Ppp.ppp_ast
    "let d = { 1, 2, false };
     show d{ 0 } * d{ 1 } + d{ 2 };";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: expected type IntType but got BoolType |}]

let%expect_test "simple-stmt-28" =
  Ppp.ppp_ast
    "let tuple = 10;
     show tuple{ 0 };";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: expected base type of CrossT but got IntType |}]

let%expect_test "simple-stmt-29" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11.] 5.;";
  [%expect
    {|
      line: 1 column: 27
      	~~ type: expected type IntType but got FloatType |}]

let%expect_test "simple-stmt-30" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11] i * j;
     show a[0., 0.];";
  [%expect
    {|
      line: 2 column: 13
      	~~ type: expected type IntType but got FloatType |}]

let%expect_test "simple-stmt-31" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11] i * j;
     show a[0, 0] && false;";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: expected type BoolType but got IntType |}]

let%expect_test "simple-stmt-32" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11] i * j;
     show a[0, 0] && false;";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: expected type BoolType but got IntType |}]

let%expect_test "simple-stmt-33" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11] i * j;
     show a[ 0 ];";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: incorrect number of indexes provided for array of rank 2 |}]

let%expect_test "simple-stmt-34" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11]
       array[k : i * j]
         array[g : 100, h : k * i] 0;
     show a[0, 0][1000][0];";
  [%expect
    {|
      line: 4 column: 11
      	~~ type: incorrect number of indexes provided for array of rank 2 |}]

let%expect_test "simple-stmt-35" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11] false;
     show a[0, 0, 0];";
  [%expect
    {|
      line: 2 column: 11
      	~~ type: incorrect number of indexes provided for array of rank 2 |}]

let%expect_test "simple-stmt-36" =
  Ppp.ppp_ast
    "fn id( x : int ) : int { return x; }
    show id( 10. );
    fn id( x : float ) : float { return x; }
    show id( 10 );";
  [%expect
    {|
      line: 2 column: 10
      	~~ type: expected parameter types of (IntType) but got (FloatType) |}]

let%expect_test "simple-stmt-37" =
  Ppp.ppp_ast
    " fn foo ( img[W,H] : float4[,,] ) : int {
        return W * H;
      }
      read image \"foo.png\" to img;
      show foo(img);";
  [%expect
    {|
      line: 1 column: 11
      	~~ type: binding a rank 3 array with 2 dimensions |}]

let%expect_test "simple-stmt-38" =
  Ppp.ppp_ast
    " fn foo ( img[W,H] : float4[] ) : int {
        return W * H;
      }
      read image \"foo.png\" to img;
      show foo(img);";
  [%expect
    {|
      line: 1 column: 11
      	~~ type: binding a rank 1 array with 2 dimensions |}]
