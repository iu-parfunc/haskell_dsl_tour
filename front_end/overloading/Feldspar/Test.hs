{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}

module Feldspar.Test where

import Feldspar.GADT
import Feldspar.InterpGADT as GADTInterp

----------------------------- Tests ---------------------------------------

test1 :: Exp () (Int -> Int -> Int)
test1 = Abs Int (Abs Int (Var Zro `Add` Var (Suc Zro)))

test2 :: Exp () Int
test2 = (App (Abs Int (App (Abs Int (Var Zro `Add` Var (Suc Zro))) (Con 1))) (Con 2))

result2 :: Int
result2 = GADTInterp.run test2 ()
