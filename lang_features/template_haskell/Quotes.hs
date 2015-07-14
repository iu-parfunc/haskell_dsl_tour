{-# LANGUAGE TemplateHaskell #-}
        -- |

module Quotes where

import Language.Haskell.TH

foo :: Q Exp
foo  = [| \n -> show n |]

baz :: Q (TExp Int)
baz = [||  3 + 3  ||]

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
