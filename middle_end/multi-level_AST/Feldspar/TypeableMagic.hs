{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators, Rank2Types, MagicHash #-}
                                                                                 -- | This is a *trusted* module which implements a mechanism for
-- Typeable dictionary recovery.

module Feldspar.TypeableMagic (recoverTypeable) where

import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)
-- import Debug.Trace
import GHC.Prim (Proxy#)

newtype Magic ans = Magic (forall a. (Typeable a) => Proxy a -> ans)
newtype Voodoo = Voodoo (forall a. Proxy# a -> TypeRep)

-- | For a given TypeRep, there must have been a Typeable dictionary.
--   This allows you to recover it.
recoverTypeable :: TypeRep -> (forall a. (Typeable a) => Proxy a -> ans) -> ans
recoverTypeable rep f = unsafeCoerce (Magic f) (Voodoo (\_ -> rep)) Proxy
