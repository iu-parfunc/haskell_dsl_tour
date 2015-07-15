{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators, Rank2Types, MagicHash #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Exposes an interface for deconstructing typeable *instances*.
--   This module only performs this trick for arrows and 2-tuples.

module Feldspar.TypeCase
  ( typeCaseArrow
  , TypeCaseArrow(..)
  , typeCaseTuple
  , TypeCaseTuple(..)
  , typeCaseTimes
  , TypeCaseTimes(..)
  , typeCaseTimess
  , TypeCaseTimess(..)
  ) where

import Data.Typeable
import Feldspar.TypeableMagic

_main :: IO ()
_main = do
  print (example1 (length :: String -> Int) "abc") -- Just 3
  print (example1 (length :: [Int]  -> Int) "abc") -- Nothing

example1 :: forall arr a. (Typeable arr, Typeable a) => arr -> a -> Maybe Int
example1 arr a = do
  -- Check that "arr" is a function from the type of "a" to Int
  TypeCaseArrow (Refl :: arr :~: (b -> c)) <- typeCaseArrow
  Refl                :: b   :~: a         <- gcast Refl
  Refl                :: c   :~: Int       <- gcast Refl
  return (arr a)

-- | Witness the arrow-ness of a type.
data TypeCaseArrow a where
  TypeCaseArrow :: (Typeable b, Typeable c) =>
                   (a :~: (b -> c)) -> TypeCaseArrow a

-- | Witness the tuple-ness of a type.
data TypeCaseTuple a where
  TypeCaseTuple :: (Typeable b, Typeable c) =>
                   (a :~: (b,c)) -> TypeCaseTuple a

-- | Test a Typeable type to see if it is an arrow.  If so, return a
-- data structure capable of witnessing that fact for the GHC type checker.
typeCaseArrow :: forall arr. (Typeable arr) => Maybe (TypeCaseArrow arr)
typeCaseArrow = case splitTyConApp (typeRep (Proxy :: Proxy arr)) of
  (op, [b,c]) | op == typeRepTyCon (typeRep (Proxy :: Proxy (->)))
              -> recoverTypeable b (\(_ :: Proxy b) ->
                 recoverTypeable c (\(_ :: Proxy c) ->
                 fmap TypeCaseArrow (gcast Refl :: Maybe (arr :~: (b -> c)))))
  _ -> Nothing
-- | Ditto for tuples.
typeCaseTuple :: forall arr. (Typeable arr) => Maybe (TypeCaseTuple arr)
typeCaseTuple = case splitTyConApp (typeRep (Proxy :: Proxy arr)) of
  (op, [b,c]) | op == typeRepTyCon (typeRep (Proxy :: Proxy (,)))
              -> recoverTypeable b (\(_ :: Proxy b) ->
                 recoverTypeable c (\(_ :: Proxy c) ->
                 fmap TypeCaseTuple (gcast Refl :: Maybe (arr :~: (b,c)))))
  _ -> Nothing

---------------------------- Typecase on user defined data type -----------

data Times t1 t2 = Times t1 t2 deriving (Typeable)

data TypeCaseTimes a where
  TypeCaseTimes :: (Typeable b, Typeable c) =>
                   (a :~: (Times b c)) -> TypeCaseTimes a

typeCaseTimes :: forall arr. (Typeable arr) => Maybe (TypeCaseTimes arr)
typeCaseTimes = case splitTyConApp (typeRep (Proxy :: Proxy arr)) of
  (op, [b,c]) | op == typeRepTyCon (typeRep (Proxy :: Proxy (Times)))
              -> recoverTypeable b (\(_ :: Proxy b) ->
                 recoverTypeable c (\(_ :: Proxy c) ->
                 fmap TypeCaseTimes (gcast Refl :: Maybe (arr :~: (Times b c)))))
  _ -> Nothing

--------------------------- Typecase on user defined GADT -----------------

data Timess t1 t2 where
  Timess :: t1 ->  t2 -> Int -> Timess t1 t2
  deriving (Typeable)

data TypeCaseTimess a where
  TypeCaseTimess :: (Typeable b, Typeable c) =>
                   (a :~: (Timess b c)) -> TypeCaseTimess a

typeCaseTimess :: forall arr. (Typeable arr) => Maybe (TypeCaseTimess arr)
typeCaseTimess = case splitTyConApp (typeRep (Proxy :: Proxy arr)) of
  (op, [b,c]) | op == typeRepTyCon (typeRep (Proxy :: Proxy (Timess)))
              -> recoverTypeable b (\(_ :: Proxy b) ->
                 recoverTypeable c (\(_ :: Proxy c) ->
                 fmap TypeCaseTimess (gcast Refl :: Maybe (arr :~: (Timess b c)))))
  _ -> Nothing
