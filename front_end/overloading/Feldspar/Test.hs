{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}

module Feldspar.Test where

import qualified Feldspar.GADT       as GADT
import Feldspar.InterpGADT as GADTInterp

----------------------------- Tests ---------------------------------------

test1 :: GADT.Exp () (Int -> Int -> Int)
test1 = GADT.Abs GADT.Int (GADT.Abs GADT.Int (GADT.Var GADT.Zro `GADT.Add` GADT.Var (GADT.Suc GADT.Zro)))

test2 :: GADT.Exp () Int
test2 = (GADT.App (GADT.Abs GADT.Int (GADT.App (GADT.Abs GADT.Int (GADT.Var GADT.Zro `GADT.Add` GADT.Var (GADT.Suc GADT.Zro))) (GADT.Con 1))) (GADT.Con 2))

result2 :: Int
result2 = GADTInterp.run test2 ()
