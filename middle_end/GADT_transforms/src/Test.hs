{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test where

import AST
import Type
import Pretty ()
import Array.Sugar
import Substitution
import Interpreter
import Fusion


-- EXERCISES
-- =========
--
-- Try to get the following expressions to fuse! You will need to
-- complete the definitions in Substitution.hs and Fusion.hs.
--
-- You can evaluate the expressions with 'run'.
--

-- function composition
--
ex01 :: Fun () (Int -> Float)
ex01 = f2 `compose` f1


-- map/map fusion
--
ex02 :: Acc (Vector Float)
ex02
  = fuseAcc
  $ Map f2 (Map f1 a3)


-- map/generate fusion
--
ex03 :: Acc (Array DIM2 Float)
ex03
  = fuseAcc
  $ Map f2 (Map f1 (Generate (constant (Z:.2:.2)) f5))


-- removing obstacles to fusion
--
ex04 :: Acc (Array DIM1 Float)
ex04
  = fuseAcc
  $ Map f2 (Alet a3 (Map f1 (Avar ZeroIdx)))


-- Example program fragments: construct your own tests!
-- ====================================================

-- Array computations

a0 :: Vector Int
a0 = fromList (Z :. 10) [1 .. 10]

a1 :: Array DIM2 Float
a1 = fromList (Z :. 5 :. 2) [1 .. 10]

a2 :: Array DIM1 (Int,Bool)
a2 = fromList (Z :. 10) [ (x, x `mod` 5 == 0) | x <- [1,3 .. 20] ]

a3 :: Acc (Vector Int)
a3 = Use a0

a4 :: Acc (Array DIM2 Float)
a4 = Use a1

a5 :: Acc (Vector (Int,Bool))
a5 = Use a2

a6 :: Acc (Vector Int)
a6 = Map f1 a3

a7 :: Acc (Vector Float)
a7 = Map f2 a6

-- TODO: map/map fusion
a8 :: Acc (Vector Float)
a8 = Map f2
   $ Map f1 a3

a9 :: Acc (Array DIM2 Int)
a9 = Generate (constant (Z:.2:.2)) f5

-- TODO: map/generate fusion
a10 :: Acc (Array DIM2 Float)
a10 = Map f2
    $ Map f1
    $ a9

a11 :: Acc (Array DIM2 Float)
a11 = Alet a10
    $ Generate (constant (Z:.2:.2)) f4


-- Scalar functions

f1 :: OpenFun env aenv (Int -> Int)
f1 = Lam $ Body $ PrimApp (PrimAdd (IntegralNumType (TypeInt IntegralDict)))
                          (Prod $ EmptyProd `PushProd` Var ZeroIdx
                                            `PushProd` Const 1)

f2 :: OpenFun env aenv (Int -> Float)
f2 = Lam $ Body $ PrimApp PrimToFloat (Var ZeroIdx)

f3 :: OpenFun env aenv (Int -> Float)
f3 = f2 `compose` f1

f4 :: Elt e => OpenFun env (aenv, Array DIM2 e) (DIM2 -> e)
f4 = Lam $ Body $ Index ZeroIdx (Var ZeroIdx)

f5 :: Shape sh => OpenFun env aenv (sh -> Int)
f5 = Lam $ Body $ constant 0


-- Scalar expressions

constant :: Elt t => t -> OpenExp env aenv t
constant = Const . fromElt

e0 :: OpenExp env aenv Int
e0 = If (constant True) (constant 3) (constant 4)

e1 :: OpenExp env aenv Int
e1 = Let (constant 5) (Var ZeroIdx)

e2 :: OpenExp env aenv Int
e2 = If (constant True) (constant 11) e1

e3 :: OpenExp env aenv Int
e3 = Let (constant 5) (If (constant True) (Var ZeroIdx) (constant 4))

e4 :: OpenExp env aenv Int
e4 = Let (constant 4)
   $ Let (constant 5)
   $ PrimApp (PrimAdd (IntegralNumType (TypeInt IntegralDict)))
             (Prod $ EmptyProd `PushProd` Var ZeroIdx
                               `PushProd` Var (SuccIdx ZeroIdx))

e5 :: OpenExp env aenv Bool
e5 = constant True

e6 :: OpenExp env aenv Bool
e6 = Let e5
   $ If (Var ZeroIdx) (constant False)
                      (Var ZeroIdx)

e7 :: OpenExp env aenv (Int,Float)
e7 = constant (1,pi)

e8 :: OpenExp env aenv Float
e8 = Let e7
   $ Prj ZeroProdIdx (Var ZeroIdx)

e9 :: OpenExp env aenv Float
e9 = Let (constant (pi, 8, 4.86))
   $ Let (PrimApp (PrimMul (FloatingNumType (TypeFloat FloatingDict)))
                  (Prod (EmptyProd `PushProd` Prj ZeroProdIdx (Var ZeroIdx)
                                   `PushProd` PrimApp PrimToFloat (Prj (SuccProdIdx ZeroProdIdx) (Var ZeroIdx)))))
         (PrimApp (PrimAdd (FloatingNumType (TypeFloat FloatingDict)))
                  (Prod (EmptyProd `PushProd` Var ZeroIdx
                                   `PushProd` Prj (SuccProdIdx (SuccProdIdx ZeroProdIdx)) (Var (SuccIdx ZeroIdx)))))

e10 :: OpenExp env aenv (Int, (Float, Int), Bool)
e10 = constant (1, (4,2), True)

e11 :: OpenExp env aenv (Float,Int)
e11 = Let e10
    $ Prj (SuccProdIdx ZeroProdIdx) (Var ZeroIdx)

e12 :: OpenExp env aenv DIM1
e12 = constant (Z :. 10)

e13 :: OpenExp env aenv (Any DIM2)
e13 = constant Any

e14 :: OpenExp env aenv ((Bool,Int), Float)
e14 = constant ((True,42), pi)

e15 :: OpenExp env aenv (Bool,Int,Float)
e15 = constant (True,1,2)

e16 :: OpenExp env aenv (Bool, (Int,Float))
e16 = constant (False, (12,42))

e17 :: OpenExp env aenv Float
e17 = Let e3
    $ PrimApp PrimToFloat (Var ZeroIdx)

e18 :: OpenExp env aenv DIM1
e18 = Let (constant (4 :: Int, Z:.10 :: DIM1))
    $ Prj ZeroProdIdx (Var ZeroIdx)

