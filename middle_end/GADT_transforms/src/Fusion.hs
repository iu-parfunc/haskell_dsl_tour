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
    Alet bnd body       -> error "TODO: fuseAcc/Alet"
    Map f xs            -> error "TODO: fuseAcc/Map"
    Generate sh f       -> error "TODO: fuseAcc/Generate"

