{-# LANGUAGE GADTs #-}

-- | Fusion transformation
--
module Fusion where

import AST
import Substitution


-- | Implement operator fusion by rewriting the AST.
--
fuseAcc :: OpenAcc aenv a -> OpenAcc aenv a
fuseAcc acc =
  case acc of
    Use xs              -> Use xs
    Avar ix             -> Avar ix
    Generate sh f       -> error "TODO: fuseAcc/Generate"
    Alet bnd body       -> error "TODO: fuseAcc/Alet"
    Map f a             -> error "TODO: fuseAcc/Map"
    -- HINT:
    --  1. Use the helper functions defined in Substitution in order to combine expressions.
    --  2. When you spot a case for fusion, don't forget to traverse down to the subterms!

