(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let%expect_test "simple-cmd-1" =
  Ppp.ppp_ast
    "read image \"someimg.png\" to img;";
  [%expect
    {|
      (Prog
       ((ReadImageCmd someimg.png
         (VarArg
          (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img)))) |}]

let%expect_test "simple-cmd-2" =
  Ppp.ppp_ast
    "read image \"someimg.png\" to img;
     write image img to \"newimg.png\";";
  [%expect
    {|
      (Prog
       ((ReadImageCmd someimg.png
         (VarArg
          (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img))
        (WriteImageCmd
         (VarExpr
          (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img)
         newimg.png))) |}]

let%expect_test "simple-stmt-1" =
  Ppp.ppp_ast
    "assert false, \"print me!\";";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType (AssertStmt UnitType (FalseExpr BoolType) "print me!")))) |}]

let%expect_test "simple-stmt-2" =
  Ppp.ppp_ast
    "let x = 1;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType x))
          (IntExpr IntType 1))))) |}]

let%expect_test "simple-stmt-3" =
  Ppp.ppp_ast
    "read image \"myimage.png\" to img;
     let arr[H, W] = img;
     let c = -H;";
  [%expect
    {|
      (Prog
       ((ReadImageCmd myimage.png
         (VarArg
          (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue
           (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2)
           (ArraybindArg
            (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) arr
            (H W)))
          (VarExpr
           (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType c))
          (UnopExpr IntType - (VarExpr IntType H)))))) |}]

let%expect_test "simple-stmt-4" =
  Ppp.ppp_ast
    "read image \"myimage.png\" to img;
     let arr[H, W] = img;
     let c = H * W;";
  [%expect
    {|
      (Prog
       ((ReadImageCmd myimage.png
         (VarArg
          (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue
           (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2)
           (ArraybindArg
            (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) arr
            (H W)))
          (VarExpr
           (ArrayType (CrossType (FloatType FloatType FloatType FloatType)) 2) img)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType c))
          (BinopExpr IntType (VarExpr IntType H) * (VarExpr IntType W)))))) |}]
