
module Test where

import AST
import Type
import Pretty ()
import Array.Sugar


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


f1 :: OpenFun env aenv (Int -> Int)
f1 = Lam $ Body $ PrimApp (PrimAdd (IntegralNumType (TypeInt IntegralDict)))
                          (Prod $ EmptyProd `PushProd` Var ZeroIdx
                                            `PushProd` Const 1)

f2 :: OpenFun env aenv (Int -> Float)
f2 = Lam $ Body $ PrimApp PrimToFloat (Var ZeroIdx)


