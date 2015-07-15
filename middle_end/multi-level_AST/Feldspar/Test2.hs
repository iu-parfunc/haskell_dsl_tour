{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}

module Feldspar.Test2 where

import           Data.Typeable
import           Text.Printf
import qualified Feldspar.GADT       as GADT
import qualified Feldspar.ManualADT  as ADT
import qualified Feldspar.InterpADT  as ADTInterp
import qualified Feldspar.InterpGADT as GADTInterp

----------------------------- Tests ---------------------------------------

test1 :: GADT.Exp () (Int -> Int -> Int)
test1 = GADT.Abs GADT.Int (GADT.Abs GADT.Int (GADT.Var GADT.Zro `GADT.Add` GADT.Var (GADT.Suc GADT.Zro)))

test2 :: GADT.Exp () Int
test2 = (GADT.App (GADT.Abs GADT.Int (GADT.App (GADT.Abs GADT.Int (GADT.Var GADT.Zro `GADT.Add` GADT.Var (GADT.Suc GADT.Zro))) (GADT.Con 1))) (GADT.Con 2))

testADT :: ADT.Exp -> Int
testADT adt =
  case ADT.upExp adt of
    Right (ADT.SealedExp e)
       | Just gadt <- gcast e -> GADTInterp.run gadt ()
    _                        -> error "up conversion failed"

-- TODO: We have problems unpacking from a SealedExp since we don't have
-- enough inherent constraints to make Haskell happy unifing the various
-- types that it needs to. We need to probably do the same business here
-- that we did elsewhere in order to give Haskell enough info to allow it
-- to unify them for us.
roundtrip :: forall e a. (Typeable e) => String -> GADT.Exp e a -> IO ()
roundtrip name gadt = do
  let adt         = ADT.downExp gadt
  printf "Test %s:\n"     name
  printf "  Evaled: %s\n" (show (testADT adt))
  printf "  DownCvt: %s\n"   (show adt)
  case (ADTInterp.run adt []) of
    ADTInterp.Rgt a -> printf "  Evaled: %s\n" (show a)
    _               -> printf " Failed to evaluate Expression: %s\n" (show adt)

foo, bar :: IO ()
foo = roundtrip "foo" test1
bar = roundtrip "bar" test2
