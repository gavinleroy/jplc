(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "hello world" =
  Ppp.ppp_ast
    "print \"Hallo, Welt!\";";
  [%expect
    {| (Prog ((PrintCmd "Hallo, Welt!"))) |}]

let%expect_test "simple cmd 1" =
  Ppp.ppp_ast
    "return float(i + j) / float(W + H);";
  [%expect
    {|
      (Prog ((StmtCmd
        (ReturnStmt
          (BinopExpr (AppExpr float (BinopExpr (VarExpr i) + (VarExpr j))) /
          (AppExpr float (BinopExpr (VarExpr W) + (VarExpr H))))))) |}]
