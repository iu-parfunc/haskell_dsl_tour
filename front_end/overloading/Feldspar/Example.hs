
module Feldspar.Example
  where

import Feldspar.GADT


fib :: Int -> Exp env Int
fib 0 = 0
fib 1 = 1
fib n =
  let_ (fib (n-1)) $
  let_ (fib (n-2)) $
    Var Zro + Var (Suc Zro)     -- ugh, de Bruijn indices by hand


fact :: Int -> Exp env Int
fact 0 = 1
fact n = constant n * fact (n-1)


-- a.k.a. binomial coefficients. n >= k > 0
pascal :: Int -> Int -> Exp env Int
pascal _ 0          = 1
pascal n k | k == n = 1
pascal n k          =
  let_ (pascal (n-1) k)     $
  let_ (pascal (n-1) (k-1)) $
    Var Zro + Var (Suc Zro)

