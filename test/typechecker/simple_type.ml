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

let%expect_test "simple-stmt-8" =
  Ppp.ppp_ast
    "let { a, b, x } = { 10, true, 3.14 };
     show a * 2;
     show b || false;
     show x % 2.6;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (CrossbindLV (CrossType (IntType BoolType FloatType))
           ((ArgLValue IntType (VarArg IntType a))
            (ArgLValue BoolType (VarArg BoolType b))
            (ArgLValue FloatType (VarArg FloatType x))))
          (CrossExpr (CrossType (IntType BoolType FloatType))
           ((IntExpr IntType 10) (TrueExpr BoolType) (FloatExpr FloatType 3.14)))))
        (ShowCmd (BinopExpr IntType (VarExpr IntType a) * (IntExpr IntType 2)))
        (ShowCmd
         (IteExpr (VarExpr BoolType b) BoolType (TrueExpr BoolType)
          (FalseExpr BoolType)))
        (ShowCmd
         (BinopExpr FloatType (VarExpr FloatType x) % (FloatExpr FloatType 2.6))))) |}]

let%expect_test "simple-stmt-9" =
  Ppp.ppp_ast
    "let a = 1;
     let b = 2.;
     let c = false;
     let d = { a, b, c };
     let { e, f, g } = d;
     show e <= (f as int) || g;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType a))
          (IntExpr IntType 1)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue FloatType (VarArg FloatType b))
          (FloatExpr FloatType 2)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue BoolType (VarArg BoolType c))
          (FalseExpr BoolType)))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (CrossType (IntType FloatType BoolType))
           (VarArg (CrossType (IntType FloatType BoolType)) d))
          (CrossExpr (CrossType (IntType FloatType BoolType))
           ((VarExpr IntType a) (VarExpr FloatType b) (VarExpr BoolType c)))))
        (StmtCmd UnitType
         (LetStmt UnitType
          (CrossbindLV (CrossType (IntType FloatType BoolType))
           ((ArgLValue IntType (VarArg IntType e))
            (ArgLValue FloatType (VarArg FloatType f))
            (ArgLValue BoolType (VarArg BoolType g))))
          (VarExpr (CrossType (IntType FloatType BoolType)) d)))
        (ShowCmd
         (IteExpr
          (BinopExpr BoolType (VarExpr IntType e) <=
           (CastExpr FloatType (VarExpr FloatType f) FloatType))
          BoolType (TrueExpr BoolType) (VarExpr BoolType g))))) |}]

let%expect_test "simple-stmt-10" =
  Ppp.ppp_ast
    "fn unpack( { a : int, b : float, c : bool } ) : bool
     { return a <= (b as int) || c; }
     let a = 1;
     let b = 2.;
     let c = false;
     let d = { a, b, c };
     show unpack( d );";
  [%expect
    {|
      (Prog
       ((Func (ArrowType BoolType ((CrossType (IntType FloatType BoolType))))
         unpack
         ((CrossBinding (CrossType (IntType FloatType BoolType))
           ((ArgBinding IntType (VarArg IntType a) IntType)
            (ArgBinding FloatType (VarArg FloatType b) FloatType)
            (ArgBinding BoolType (VarArg BoolType c) BoolType))))
         BoolType
         ((ReturnStmt BoolType UnitType
           (IteExpr
            (BinopExpr BoolType (VarExpr IntType a) <=
             (CastExpr FloatType (VarExpr FloatType b) FloatType))
            BoolType (TrueExpr BoolType) (VarExpr BoolType c)))))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType a))
          (IntExpr IntType 1)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue FloatType (VarArg FloatType b))
          (FloatExpr FloatType 2)))
        (StmtCmd UnitType
         (LetStmt UnitType (ArgLValue BoolType (VarArg BoolType c))
          (FalseExpr BoolType)))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (CrossType (IntType FloatType BoolType))
           (VarArg (CrossType (IntType FloatType BoolType)) d))
          (CrossExpr (CrossType (IntType FloatType BoolType))
           ((VarExpr IntType a) (VarExpr FloatType b) (VarExpr BoolType c)))))
        (ShowCmd
         (AppExpr BoolType unpack
          ((VarExpr (CrossType (IntType FloatType BoolType)) d)))))) |}]

let%expect_test "simple-stmt-11" =
  Ppp.ppp_ast
    "let a = sum [i : 100] i;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType (ArgLValue IntType (VarArg IntType a))
          (SumExpr IntType ((i (IntExpr IntType 100))) (VarExpr IntType i)))))) |}]

let%expect_test "simple-stmt-12" =
  Ppp.ppp_ast
    "let a = array [i : 100, j : 100] i + j;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType IntType 2) (VarArg (ArrayType IntType 2) a))
          (ArrayExpr (ArrayType IntType 2)
           ((i (IntExpr IntType 100)) (j (IntExpr IntType 100)))
           (BinopExpr IntType (VarExpr IntType i) + (VarExpr IntType j))))))) |}]

let%expect_test "simple-stmt-13" =
  Ppp.ppp_ast
    "let { M, N } = { 100, 1000 };
     let a = array [i : M, j : N * 10] 3 + j * i;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (CrossbindLV (CrossType (IntType IntType))
           ((ArgLValue IntType (VarArg IntType M))
            (ArgLValue IntType (VarArg IntType N))))
          (CrossExpr (CrossType (IntType IntType))
           ((IntExpr IntType 100) (IntExpr IntType 1000)))))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType IntType 2) (VarArg (ArrayType IntType 2) a))
          (ArrayExpr (ArrayType IntType 2)
           ((i (VarExpr IntType M))
            (j (BinopExpr IntType (VarExpr IntType N) * (IntExpr IntType 10))))
           (BinopExpr IntType (IntExpr IntType 3) +
            (BinopExpr IntType (VarExpr IntType j) * (VarExpr IntType i)))))))) |}]
