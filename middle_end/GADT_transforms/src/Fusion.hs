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
    Generate sh f       -> Generate sh f
    Alet bnd body       -> Alet (fuseAcc bnd) (fuseAcc body)
    Map f a             ->
      case fuseAcc a of
        Map g b         -> Map (f `compose` g) b
        Generate sh g   -> Generate sh (f `compose` g)
        a'              -> Map f a'

