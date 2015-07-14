{-# LANGUAGE TemplateHaskell #-}

{-|

See wiki entry here:

  https://wiki.haskell.org/Template_Haskell#What_is_Template_Haskell.3F

This is a tiny example of how to use template Haskell.

 -}

module Main where

import Language.Haskell.TH ()
import TemplateHaskell1

-- The simplest thing we can do with something of type `Q Exp` is to
-- splice it in.
bar :: Int -> String
bar = $(foo)

-- To splice in a typed quotation we use a slightly different syntax:
quux :: Int
quux = $$baz

main :: IO ()
main = print (bar quux)

-- Now, if we want to inspect those ASTs to programatically transform
-- them, then we need to understand the datatype for ASTs.

-- The TemplateHaskell representation of Haskell source has 22 cases
-- in `Exp`.  This is somewhat less than the (extremely detailed)
-- haskell-src-ext representation of parsed code, which has 52
-- cases.

-- Still, a TH AST includes do notation, comprehensions, .. sequences,
-- if and multi-if, list literals, infix applications, and record
-- update syntax.

-- The "th-desugar" package (which "singletons" uses) addresses this.
-- It replaces those fatter sum types with the svelt one below with
-- only nine cases!
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
