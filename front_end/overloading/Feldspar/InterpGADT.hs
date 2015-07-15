{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RoleAnnotations #-}

module Feldspar.InterpGADT where

import Feldspar.GADT

-- Extraction of values form environment
get :: Var e a -> e -> a
get Zro     (_ ,x) = x
get (Suc n) (xs,_) = get n xs

-- Extraction of values form environment with singletons
gets :: Var e a -> Env e -> Typ a
gets Zro     (Ext _  x) = x
gets (Suc n) (Ext xs _) = gets n xs
gets _       Emp        = error "Impossible!"

-- Evaluation of expressions under specific environment of values
run :: Exp e a -> e -> a
run (Con i)     _ = i
run (Var x)     r = get x r
run (Abs _  eb) r = \v -> run eb (r,v)
run (App ef ea) r = run ef r $ run ea r
run (Add el er) r = run el r + run er r
run (Mul el er) r = run el r * run er r

-- Typechecking and returning the type, if successful
chk :: Exp e a -> Env e -> Typ a
chk (Con _)     _ = Int
chk (Var x)     r = gets x r
chk (Abs ta eb) r = ta `Arr` chk eb (r `Ext` ta)
chk (App ef _ ) r = case chk ef r of
                      Arr _ tr -> tr
chk (Add _  _ ) _ = Int
chk (Mul _  _ ) _ = Int


-- Count the number of terms in an expression
numberOfTerms :: Exp env a -> Int
numberOfTerms = cnt
  where
    cnt :: Exp env a -> Int
    cnt Con{}           = 1
    cnt (Var ix)        = 1 + idxToInt ix + 1   -- indices start from zero
    cnt (Abs _ e)       = 1 + cnt e             -- include terms making up the type?
    cnt (App f x)       = 1 + cnt f + cnt x
    cnt (Add x y)       = 1 + cnt x + cnt y
    cnt (Mul x y)       = 1 + cnt x + cnt y


-- Examples
-- --------

-- An example expression doubling the input number
dbl :: Exp env (Int -> Int)
dbl = Abs Int (Var Zro `Add` Var Zro)

-- An example expression composing two types
compose :: (Elt a, Elt b, Elt c) => Exp env ((b -> c) -> (a -> b) -> (a -> c))
compose
  = Abs eltType
  $ Abs eltType
  $ Abs eltType (Var (Suc (Suc Zro)) `App` (Var (Suc Zro) `App` Var Zro))

-- An example expression representing the Integer 4
four :: Exp () Int
four = (compose `App` dbl `App` dbl) `App` Con 1

-- Two test cases
test :: Bool
test = (case chk four Emp of
          Int -> True)
       &&
       (run four () == 4)

