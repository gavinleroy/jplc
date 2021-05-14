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
    "return (i + j as float) / (W + H as float);";
  [%expect
    {|
      (Prog
       ((StmtCmd
         (ReturnStmt
          (BinopExpr (CastExpr (BinopExpr (VarExpr i) + (VarExpr j)) FloatType) /
           (CastExpr (BinopExpr (VarExpr W) + (VarExpr H)) FloatType)))))) |}]

let%expect_test "simple-cmd-3" =
  Ppp.ppp_ast
    "// Ok
     read image \"photo.png\" to photo_image;";
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

let%expect_test "simple-cmd-5" =
  Ppp.ppp_ast
    " let {a,b,c} = {1, 2, 3};";
  [%expect
    {|
      (Prog
       ((StmtCmd
         (LetStmt
          (CrossbindLV
           ((ArgLValue (VarArg a)) (ArgLValue (VarArg b)) (ArgLValue (VarArg c))))
          (CrossExpr ((IntExpr 1) (IntExpr 2) (IntExpr 3)))))))|}]

(* write some test about state array initialization*)
let%expect_test "simple-cmd-6" =
  Ppp.ppp_ast
    "show [1, 2, x];";
  [%expect
    {|
      (Prog ((ShowCmd (ArrayConsExpr ((IntExpr 1) (IntExpr 2) (VarExpr x))))))|}]

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
             (UnopExpr -
              (UnopExpr - (UnopExpr - (UnopExpr - (UnopExpr - (IntExpr 3))))))))))))) |}]

let%expect_test "simple-op-2" =
  Ppp.ppp_ast
    "// OK
    let y = !!!!!!!!!!5;";
  [%expect
    {|
      (Prog
       ((StmtCmd
         (LetStmt (ArgLValue (VarArg y))
          (UnopExpr !
           (UnopExpr !
            (UnopExpr !
             (UnopExpr !
              (UnopExpr !
               (UnopExpr !
                (UnopExpr ! (UnopExpr ! (UnopExpr ! (UnopExpr ! (IntExpr 5))))))))))))))) |}]
