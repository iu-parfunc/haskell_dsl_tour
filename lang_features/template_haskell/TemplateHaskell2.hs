{-# LANGUAGE TemplateHaskell #-}

{-|

See wiki entry here:

  https://wiki.haskell.org/Template_Haskell#What_is_Template_Haskell.3F

This is a tiny example of how to use template Haskell.

 -}

module Main where

import Language.Haskell.TH
import TemplateHaskell1

-- The simplest thing we can do with something of type `Q Exp` is to
-- splice it in.
bar :: Int -> String
bar = $(foo)

-- To splice in a typed quotation we use a slightly different syntax:
quux :: Int
quux = $$baz

main :: IO ()
main = print "hi"


-- 22 case Exps in TH
--  (Versus 52 for haskell-src-ext)

-- Includes do notation, comprehensions, .. sequences, if and
-- multi-if, list literals, infix applications, record update syntax.

-- "th-desugar" package addresses this.
-- singletons lib uses it.

{-
   data DExp = DVarE Name
       | DConE Name
       | DLitE Lit
       | DAppE DExp DExp
       | DLamE [Name] DExp
       | DCaseE DExp [DMatch]
       | DLetE [DLetDec] DExp
       | DSigE DExp DType
       | DStaticE DExp

   -}
