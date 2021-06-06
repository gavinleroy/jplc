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
           (BinopExpr FloatType (CastExpr IntType -> FloatType (VarExpr IntType a))
            * (VarExpr FloatType b)))))
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
           (CastExpr FloatType -> IntType (VarExpr FloatType f)))
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
             (CastExpr FloatType -> IntType (VarExpr FloatType b)))
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

let%expect_test "simple-stmt-14" =
  Ppp.ppp_ast
    "let d = { 1, 2., false };
     show d{ 0 } * 5;
     show d{ 1 } / 3.;
     show d{ 2 } && d{ 2 };";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (CrossType (IntType FloatType BoolType))
           (VarArg (CrossType (IntType FloatType BoolType)) d))
          (CrossExpr (CrossType (IntType FloatType BoolType))
           ((IntExpr IntType 1) (FloatExpr FloatType 2) (FalseExpr BoolType)))))
        (ShowCmd
         (BinopExpr IntType
          (CrossidxExpr IntType
           (VarExpr (CrossType (IntType FloatType BoolType)) d) 0)
          * (IntExpr IntType 5)))
        (ShowCmd
         (BinopExpr FloatType
          (CrossidxExpr FloatType
           (VarExpr (CrossType (IntType FloatType BoolType)) d) 1)
          / (FloatExpr FloatType 3)))
        (ShowCmd
         (IteExpr
          (CrossidxExpr BoolType
           (VarExpr (CrossType (IntType FloatType BoolType)) d) 2)
          BoolType
          (CrossidxExpr BoolType
           (VarExpr (CrossType (IntType FloatType BoolType)) d) 2)
          (FalseExpr BoolType))))) |}]

let%expect_test "simple-stmt-14" =
  Ppp.ppp_ast
    "let d = { 1, 2., false };
     let dd = { d, d, { true, true, d } };
     show dd{ 0 }{ 1 } * .5;
     let d = dd{ 2 };
     show d{ 1 } || true;
     show dd{ 2 }{ 2 }{ 0 } * 2;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (CrossType (IntType FloatType BoolType))
           (VarArg (CrossType (IntType FloatType BoolType)) d))
          (CrossExpr (CrossType (IntType FloatType BoolType))
           ((IntExpr IntType 1) (FloatExpr FloatType 2) (FalseExpr BoolType)))))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue
           (CrossType
            ((CrossType (IntType FloatType BoolType))
             (CrossType (IntType FloatType BoolType))
             (CrossType
              (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
           (VarArg
            (CrossType
             ((CrossType (IntType FloatType BoolType))
              (CrossType (IntType FloatType BoolType))
              (CrossType
               (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
            dd))
          (CrossExpr
           (CrossType
            ((CrossType (IntType FloatType BoolType))
             (CrossType (IntType FloatType BoolType))
             (CrossType
              (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
           ((VarExpr (CrossType (IntType FloatType BoolType)) d)
            (VarExpr (CrossType (IntType FloatType BoolType)) d)
            (CrossExpr
             (CrossType
              (BoolType BoolType (CrossType (IntType FloatType BoolType))))
             ((TrueExpr BoolType) (TrueExpr BoolType)
              (VarExpr (CrossType (IntType FloatType BoolType)) d)))))))
        (ShowCmd
         (BinopExpr FloatType
          (CrossidxExpr FloatType
           (CrossidxExpr (CrossType (IntType FloatType BoolType))
            (VarExpr
             (CrossType
              ((CrossType (IntType FloatType BoolType))
               (CrossType (IntType FloatType BoolType))
               (CrossType
                (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
             dd)
            0)
           1)
          * (FloatExpr FloatType 0.5)))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue
           (CrossType (BoolType BoolType (CrossType (IntType FloatType BoolType))))
           (VarArg
            (CrossType
             (BoolType BoolType (CrossType (IntType FloatType BoolType))))
            d))
          (CrossidxExpr
           (CrossType (BoolType BoolType (CrossType (IntType FloatType BoolType))))
           (VarExpr
            (CrossType
             ((CrossType (IntType FloatType BoolType))
              (CrossType (IntType FloatType BoolType))
              (CrossType
               (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
            dd)
           2)))
        (ShowCmd
         (IteExpr
          (CrossidxExpr BoolType
           (VarExpr
            (CrossType
             (BoolType BoolType (CrossType (IntType FloatType BoolType))))
            d)
           1)
          BoolType (TrueExpr BoolType) (TrueExpr BoolType)))
        (ShowCmd
         (BinopExpr IntType
          (CrossidxExpr IntType
           (CrossidxExpr (CrossType (IntType FloatType BoolType))
            (CrossidxExpr
             (CrossType
              (BoolType BoolType (CrossType (IntType FloatType BoolType))))
             (VarExpr
              (CrossType
               ((CrossType (IntType FloatType BoolType))
                (CrossType (IntType FloatType BoolType))
                (CrossType
                 (BoolType BoolType (CrossType (IntType FloatType BoolType))))))
              dd)
             2)
            2)
           0)
          * (IntExpr IntType 2))))) |}]

let%expect_test "simple-stmt-15" =
  Ppp.ppp_ast
    "let a = [ 1, 2, 3, 4 ];
     show a[0] * 10;
     show a[0] * a[1] * a[2] * a[3];";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType IntType 1) (VarArg (ArrayType IntType 1) a))
          (ArrayConsExpr (ArrayType IntType 1)
           ((IntExpr IntType 1) (IntExpr IntType 2) (IntExpr IntType 3)
            (IntExpr IntType 4)))))
        (ShowCmd
         (BinopExpr IntType
          (ArrayidxExpr IntType (VarExpr (ArrayType IntType 1) a)
           ((IntExpr IntType 0)))
          * (IntExpr IntType 10)))
        (ShowCmd
         (BinopExpr IntType
          (BinopExpr IntType
           (BinopExpr IntType
            (ArrayidxExpr IntType (VarExpr (ArrayType IntType 1) a)
             ((IntExpr IntType 0)))
            *
            (ArrayidxExpr IntType (VarExpr (ArrayType IntType 1) a)
             ((IntExpr IntType 1))))
           *
           (ArrayidxExpr IntType (VarExpr (ArrayType IntType 1) a)
            ((IntExpr IntType 2))))
          *
          (ArrayidxExpr IntType (VarExpr (ArrayType IntType 1) a)
           ((IntExpr IntType 3))))))) |}]

let%expect_test "simple-stmt-16" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 20] 1;
     show a[0, 10];";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType IntType 2) (VarArg (ArrayType IntType 2) a))
          (ArrayExpr (ArrayType IntType 2)
           ((i (IntExpr IntType 10)) (j (IntExpr IntType 20))) (IntExpr IntType 1))))
        (ShowCmd
         (ArrayidxExpr IntType (VarExpr (ArrayType IntType 2) a)
          ((IntExpr IntType 0) (IntExpr IntType 10)))))) |}]

let%expect_test "simple-stmt-17" =
  Ppp.ppp_ast
    "let a = array[i : 10] array [j : i] j * i;
     show a[0][0];";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType (ArrayType IntType 1) 1)
           (VarArg (ArrayType (ArrayType IntType 1) 1) a))
          (ArrayExpr (ArrayType (ArrayType IntType 1) 1) ((i (IntExpr IntType 10)))
           (ArrayExpr (ArrayType IntType 1) ((j (VarExpr IntType i)))
            (BinopExpr IntType (VarExpr IntType j) * (VarExpr IntType i))))))
        (ShowCmd
         (ArrayidxExpr IntType
          (ArrayidxExpr (ArrayType IntType 1)
           (VarExpr (ArrayType (ArrayType IntType 1) 1) a) ((IntExpr IntType 0)))
          ((IntExpr IntType 0)))))) |}]

let%expect_test "simple-stmt-18" =
  Ppp.ppp_ast
    "show (array[i : 10, j : 10, k : 10] false)[1,2,3];";
  [%expect
    {|
      (Prog
       ((ShowCmd
         (ArrayidxExpr BoolType
          (ArrayExpr (ArrayType BoolType 3)
           ((i (IntExpr IntType 10)) (j (IntExpr IntType 10))
            (k (IntExpr IntType 10)))
           (FalseExpr BoolType))
          ((IntExpr IntType 1) (IntExpr IntType 2) (IntExpr IntType 3)))))) |}]

let%expect_test "simple-stmt-19" =
  Ppp.ppp_ast
    "let a = array[i : 10000, j : 10000] j <= i;
     let b = array[i : 999, j : 999] a[i, j];
     show b[800, 777] || true;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType BoolType 2) (VarArg (ArrayType BoolType 2) a))
          (ArrayExpr (ArrayType BoolType 2)
           ((i (IntExpr IntType 10000)) (j (IntExpr IntType 10000)))
           (BinopExpr BoolType (VarExpr IntType j) <= (VarExpr IntType i)))))
        (StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType BoolType 2) (VarArg (ArrayType BoolType 2) b))
          (ArrayExpr (ArrayType BoolType 2)
           ((i (IntExpr IntType 999)) (j (IntExpr IntType 999)))
           (ArrayidxExpr BoolType (VarExpr (ArrayType BoolType 2) a)
            ((VarExpr IntType i) (VarExpr IntType j))))))
        (ShowCmd
         (IteExpr
          (ArrayidxExpr BoolType (VarExpr (ArrayType BoolType 2) b)
           ((IntExpr IntType 800) (IntExpr IntType 777)))
          BoolType (TrueExpr BoolType) (TrueExpr BoolType))))) |}]

let%expect_test "simple-stmt-20" =
  Ppp.ppp_ast
    "let a = array[i : 10, j : 11]
       array[k : i * j]
         array[g : 100, h : k * i] 0;
     show a[0, 0][1000][0, 10] == 0;";
  [%expect
    {|
      (Prog
       ((StmtCmd UnitType
         (LetStmt UnitType
          (ArgLValue (ArrayType (ArrayType (ArrayType IntType 2) 1) 2)
           (VarArg (ArrayType (ArrayType (ArrayType IntType 2) 1) 2) a))
          (ArrayExpr (ArrayType (ArrayType (ArrayType IntType 2) 1) 2)
           ((i (IntExpr IntType 10)) (j (IntExpr IntType 11)))
           (ArrayExpr (ArrayType (ArrayType IntType 2) 1)
            ((k (BinopExpr IntType (VarExpr IntType i) * (VarExpr IntType j))))
            (ArrayExpr (ArrayType IntType 2)
             ((g (IntExpr IntType 100))
              (h (BinopExpr IntType (VarExpr IntType k) * (VarExpr IntType i))))
             (IntExpr IntType 0))))))
        (ShowCmd
         (BinopExpr BoolType
          (ArrayidxExpr IntType
           (ArrayidxExpr (ArrayType IntType 2)
            (ArrayidxExpr (ArrayType (ArrayType IntType 2) 1)
             (VarExpr (ArrayType (ArrayType (ArrayType IntType 2) 1) 2) a)
             ((IntExpr IntType 0) (IntExpr IntType 0)))
            ((IntExpr IntType 1000)))
           ((IntExpr IntType 0) (IntExpr IntType 10)))
          == (IntExpr IntType 0))))) |}]

let%expect_test "simple-stmt-21" =
  Ppp.ppp_ast
    "fn bad_fac(x : int) : int
     { return x * bad_fac(x - 1); }
     show bad_fac(10);";
  [%expect
    {|
      (Prog
       ((Func (ArrowType IntType (IntType)) bad_fac
         ((ArgBinding IntType (VarArg IntType x) IntType)) IntType
         ((ReturnStmt IntType UnitType
           (BinopExpr IntType (VarExpr IntType x) *
            (AppExpr IntType bad_fac
             ((BinopExpr IntType (VarExpr IntType x) - (IntExpr IntType 1))))))))
        (ShowCmd (AppExpr IntType bad_fac ((IntExpr IntType 10)))))) |}]

let%expect_test "simple-stmt-22" =
  Ppp.ppp_ast
    "fn fac(x : int) : int
     { return if x == 0 then 1 else x * fac(x - 1); }
     show fac (10);";
  [%expect
    {|
      (Prog
       ((Func (ArrowType IntType (IntType)) fac
         ((ArgBinding IntType (VarArg IntType x) IntType)) IntType
         ((ReturnStmt IntType UnitType
           (IteExpr (BinopExpr BoolType (VarExpr IntType x) == (IntExpr IntType 0))
            IntType (IntExpr IntType 1)
            (BinopExpr IntType (VarExpr IntType x) *
             (AppExpr IntType fac
              ((BinopExpr IntType (VarExpr IntType x) - (IntExpr IntType 1)))))))))
        (ShowCmd (AppExpr IntType fac ((IntExpr IntType 10)))))) |}]

let%expect_test "simple-stmt-23" =
  Ppp.ppp_ast
    "fn iden(x : int) : int { return x; }
     show iden( 10 );
     fn iden(y : float) : float { return y; }
     show iden( 199. );";
  [%expect
    {|
      (Prog
       ((Func (ArrowType IntType (IntType)) iden
         ((ArgBinding IntType (VarArg IntType x) IntType)) IntType
         ((ReturnStmt IntType UnitType (VarExpr IntType x))))
        (ShowCmd (AppExpr IntType iden ((IntExpr IntType 10))))
        (Func (ArrowType FloatType (FloatType)) iden
         ((ArgBinding FloatType (VarArg FloatType y) FloatType)) FloatType
         ((ReturnStmt FloatType UnitType (VarExpr FloatType y))))
        (ShowCmd (AppExpr FloatType iden ((FloatExpr FloatType 199)))))) |}]

let%expect_test "simple-stmt-24" =
  Ppp.ppp_ast
    "show 1 + 2 * 3;";
  [%expect
    {|
      (Prog
       ((ShowCmd
         (BinopExpr IntType (IntExpr IntType 1) +
          (BinopExpr IntType (IntExpr IntType 2) * (IntExpr IntType 3)))))) |}]
