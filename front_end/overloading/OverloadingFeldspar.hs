{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module OverloadingFeldspar where

import           Feldspar.GADT
import           Feldspar.InterpGADT
import           Prelude (Int,(*),(+),fromInteger, return,(>>))
import qualified Prelude as P

x :: Exp () Int
x = 3 + 4

type EBool e = Exp e (Int -> Int -> Int)

true :: EBool e
true = Abs Int (Abs Int (Var (Suc Zro)))

false :: EBool e
false = Abs Int (Abs Int (Var Zro))

ifThenElse :: EBool e -> Exp e Int -> Exp e Int -> Exp e Int
ifThenElse b e1 e2 =
  App (App b e1) e2

y :: Exp () Int
y = if true
       then x
       else 2 * x

z :: Exp () Int
z = if false
       then x
       else 2 * x

main :: P.IO ()
main = do P.print (run y ())
          P.print (run z ())
