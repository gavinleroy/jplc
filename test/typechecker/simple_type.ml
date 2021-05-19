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

let%expect_test "simple-stmt-5" =
  Ppp.ppp_ast
    "let x = [1, 2, 3, 4, 5];";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType IntType 1) (VarArg (ArrayType IntType 1) x))
          (ArrayConsExpr (ArrayType IntType 1)
           ((IntExpr IntType 1) (IntExpr IntType 2) (IntExpr IntType 3)
            (IntExpr IntType 4) (IntExpr IntType 5))))))) |}]


let%expect_test "simple-stmt-6" =
  Ppp.ppp_ast
    "
    fn f(a : int, b : float) : float
    {
      return a as float * b;
    }
    let x = [0., f(2, 5.)];";
  [%expect
    {|
      (Prog
       ((Func (ArrowType FloatType (IntType FloatType)) f
         ((ArgBinding IntType (VarArg IntType a) IntType)
          (ArgBinding FloatType (VarArg FloatType b) FloatType))
         FloatType
         ((ReturnStmt FloatType UnitType
           (BinopExpr FloatType (CastExpr IntType (VarExpr IntType a) IntType) *
            (VarExpr FloatType b)))))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType FloatType 1) (VarArg (ArrayType FloatType 1) x))
          (ArrayConsExpr (ArrayType FloatType 1)
           ((FloatExpr FloatType 0)
            (AppExpr FloatType f ((IntExpr IntType 2) (FloatExpr FloatType 5))))))))) |}]

let%expect_test "simple-stmt-7" =
  Ppp.ppp_ast
    "let x = 10 == 10;
     show x && false;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType (ArgLValue BoolType (VarArg BoolType x))
          (BinopExpr BoolType (IntExpr IntType 10) == (IntExpr IntType 10))))
        (ShowCmd
         (IteExpr (VarExpr BoolType x) BoolType (FalseExpr BoolType)
          (FalseExpr BoolType))))) |}]
