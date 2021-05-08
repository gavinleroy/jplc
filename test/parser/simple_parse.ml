(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "simple-cmd-1" =
  Ppp.ppp_ast
    "print \"Hallo, Welt!\";";
  [%expect
    {| (Prog ((PrintCmd "Hallo, Welt!"))) |}]

let%expect_test "simple-cmd-2" =
  Ppp.ppp_ast
    "return float(i + j) / float(W + H);";
  [%expect
    {|
      (Prog
       ((StmtCmd
         (ReturnStmt
          (BinopExpr (AppExpr float ((BinopExpr (VarExpr i) + (VarExpr j)))) /
           (AppExpr float ((BinopExpr (VarExpr W) + (VarExpr H))))))))) |}]

let%expect_test "simple-cmd-3" =
  Ppp.ppp_ast
    "read image \"photo.png\" to photo_image;";
  [%expect
    {| (Prog ((ReadImageCmd photo.png (VarArg photo_image)))) |}]

let%expect_test "simple-cmd-4" =
  Ppp.ppp_ast
    "time time time time time time return 3;";
  [%expect
    {|
      (Prog
       ((TimeCmd
         (TimeCmd
          (TimeCmd
           (TimeCmd (TimeCmd (TimeCmd (StmtCmd (ReturnStmt (IntExpr 3))))))))))) |}]

let%expect_test "simple-op-1" =
    Ppp.ppp_ast
    "// OK
     let x = --------3;";
  [%expect
    {|
      (Prog
       ((StmtCmd
         (LetStmt (ArgLValue (VarArg x))
          (UnopExpr -
           (UnopExpr -
            (UnopExpr -
             (UnopExpr - (UnopExpr - (UnopExpr - (UnopExpr - (IntExpr -3)))))))))))) |}]
