{-# LANGUAGE TemplateHaskell #-}

{-|

See wiki entry here:

  https://wiki.haskell.org/Template_Haskell#What_is_Template_Haskell.3F

This is a tiny example of how to use template Haskell.  Because quotes
and splices must be in different modules, look at both this file and
TemplateHaskell2.hs.

 -}

module TemplateHaskell1 where

import Language.Haskell.TH

-- First,
foo :: Q Exp
foo  = [| \n -> show n |]

-- Second,
baz :: Q (TExp Int)
baz = [||  3 + 3  ||]
