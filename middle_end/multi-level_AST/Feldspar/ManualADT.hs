{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Handwritten GADT<->ADT conversions
--
module Feldspar.ManualADT where


import Data.Typeable
import Control.DeepSeq
import Text.Printf

import Unsafe.Coerce                    ( unsafeCoerce )
import GHC.Generics                     ( Generic )
import GHC.Prim                         ( Proxy# )

import Feldspar.TypeCase                ( typeCaseTuple, TypeCaseTuple(..) )
import qualified Feldspar.GADT          as G

------------------------------ From Ken's Example -------------------------

data TypeCaseArrow a where
  TypeCaseArrow :: (Typeable b, Typeable c) =>
                   (a :~: (b -> c)) -> TypeCaseArrow a

typeCaseArrow :: forall arr. (Typeable arr) => Maybe (TypeCaseArrow arr)
typeCaseArrow = case splitTyConApp (typeRep (Proxy :: Proxy arr)) of
  (op, [b,c]) | op == typeRepTyCon (typeRep (Proxy :: Proxy (->)))
              -> unsafeTypeable b (\(_ :: Proxy b) ->
                 unsafeTypeable c (\(_ :: Proxy c) ->
                 fmap TypeCaseArrow (gcast Refl :: Maybe (arr :~: (b -> c)))))
  _ -> Nothing

newtype Magic ans = Magic (forall a. (Typeable a) => Proxy a -> ans)
newtype Voodoo = Voodoo (forall a. Proxy# a -> TypeRep)

unsafeTypeable :: TypeRep -> (forall a. (Typeable a) => Proxy a -> ans) -> ans
unsafeTypeable rep f = unsafeCoerce (Magic f) (Voodoo (\_ -> rep)) Proxy

---------------------------------------------------------------------------

data Exp where
  Con :: Int -> Exp
  Add :: Exp -> Exp -> Exp
  Mul :: Exp -> Exp -> Exp
  Var :: Var -> Exp
  Abs :: Typ -> Exp -> Exp
  App :: Exp -> Exp -> Exp
 deriving (Show, Generic)

-- This one is subtle.  Why is the "a" param not ambiguous?  We're
-- deleting it with "synthesized" mode, but the synthesized param is
-- __determined__ by the checked param, so this should pass muster.
--
data Var where
  Zro :: Var
  Suc :: Var -> Var
  deriving (Show, Generic)

data Typ where
  Int :: Typ
  Arr :: Typ -> Typ -> Typ
 deriving (Eq, Show, Generic)

-- Because I was told to synthesize "a", I must hide it in the sealed
-- result type here:
--
data SealedExp e where
  SealedExp :: forall a e . Typeable a =>
               G.Exp e a -> SealedExp e

data SealedVar e where
  SealedVar :: Typeable a => G.Var e a -> SealedVar e

data SealedTyp where
  SealedTyp :: Typeable a => G.Typ a -> SealedTyp

instance NFData Exp
instance NFData Var
instance NFData Typ

---------------------------------------------------------------------------
-- Restoring types
---------------------------------------------------------------------------

type TypeError = String

typeError :: forall s t. (Typeable s, Typeable t) => s -> t -> TypeError
typeError _ _
  = printf "Couldn't match expected type `%s' with actual type `%s'"
           (show (typeOf (undefined::s)))
           (show (typeOf (undefined::t)))

unify :: forall s t. (Typeable s, Typeable t)
      => s
      -> t
      -> Either TypeError (s :~: t)
unify s t =
  case (eqT :: Maybe (s :~: t)) of
    Nothing   -> Left (typeError s t)
    Just Refl -> Right Refl


-- Because "env" is checked, it is a "parameter" here:
--
upExp :: forall env. Typeable env
      => Exp
      -> Either TypeError (SealedExp env)
upExp = cvt
  where
    cvt :: Exp -> Either TypeError (SealedExp env)
    cvt (Con i)         = return $ SealedExp (G.Con i :: G.Exp env Int)
    cvt (Add x y)       = do
      -- We know the "env" in the output is the same as the inputs. That lets us
      -- know what "env" to ask for in our recursive calls here.
      SealedExp (x' :: G.Exp env x)     <- cvt x
      SealedExp (y' :: G.Exp env y)     <- cvt y
      Refl                              <- unify (undefined :: x) (undefined :: Int)
      Refl                              <- unify (undefined :: y) (undefined :: Int)
      return $ SealedExp (G.Add x' y')

    cvt (Mul x y)       = do
      SealedExp (x' :: G.Exp env x)     <- cvt x
      SealedExp (y' :: G.Exp env y)     <- cvt y
      Refl                              <- unify (undefined :: x) (undefined :: y)
      Refl                              <- unify (undefined :: x) (undefined :: Int)
      return $ SealedExp (G.Mul x' y')

    cvt (Var ix)        = do
      SealedVar (ix' :: G.Var env t)    <- upVar ix
      return $ SealedExp (G.Var ix')

    cvt (Abs t e)       = do
      SealedTyp (t' :: G.Typ t)         <- upTyp t
      SealedExp (e' :: G.Exp (e,t) b)   <- upExp e
      return $ SealedExp (G.Abs t' e')

    cvt (App f x)       = do
      SealedExp (f' :: G.Exp env f)     <- upExp f
      SealedExp (x' :: G.Exp env a)     <- upExp x
      case typeCaseArrow :: Maybe (TypeCaseArrow f) of
        Nothing                                         -> Left "type error: App"
        Just (TypeCaseArrow (Refl :: f :~: (a' -> b'))) -> do
          Refl <- unify (undefined :: a) (undefined :: a')
          return $ SealedExp (G.App f' x')


-- Typechecks, but we run into problems with Typeable and guaranteeing that it's a tuple when calling this.
-- upVar :: forall a b. (Typeable a, Typeable b) => Var  -> Maybe (SealedVar (a,b))

upVar :: forall e. Typeable e
      => Var
      -> Either TypeError (SealedVar e)
upVar Zro =
  case typeCaseTuple :: Maybe (TypeCaseTuple e) of
    Nothing                                     -> Left "type error: upVar"
    Just (TypeCaseTuple (Refl :: e :~: (x,y)))  -> return $ SealedVar (G.Zro :: G.Var (x,y) y)
upVar (Suc v) =
  -- problems with unification of types here
  case typeCaseTuple :: Maybe (TypeCaseTuple e) of
    Nothing                                     -> Left "type error: upVar"
    Just (TypeCaseTuple (Refl :: e :~: (e1,b))) -> do
      SealedVar (v' :: G.Var e1 a) <- upVar v
      return $ SealedVar (G.Suc v' :: G.Var e a)

upTyp :: Typ -> Either TypeError SealedTyp
upTyp Int         = return (SealedTyp G.Int)
upTyp (Arr x1 x2) = do
  SealedTyp a   <- upTyp x1
  SealedTyp b   <- upTyp x2
  -- No constraints on a/b.  How do we ensure (a->b) on the result though?
  -- Goal: call G.Arr
  -- Reasoning: why do we not need a cast?
  return $ SealedTyp (G.Arr a b)


--------------------------------------------------------------------------------
-- Stripping types
--------------------------------------------------------------------------------

downExp :: forall env t. G.Exp env t -> Exp
downExp = cvt
  where
    cvt :: G.Exp env t -> Exp
    cvt (G.Con i)       = Con i
    cvt (G.Var ix)      = Var (cvtV ix)
    cvt (G.Add x y)     = Add (cvt x) (cvt y)
    cvt (G.Mul x y)     = Mul (cvt x) (cvt y)
    cvt (G.Abs t e)     = Abs (cvtT t) (downExp e)
    cvt (G.App f x)     = App (downExp f) (downExp x)

    cvtV :: G.Var e a -> Var
    cvtV G.Zro      = Zro
    cvtV (G.Suc ix) = Suc (cvtV ix)

    cvtT :: G.Typ a -> Typ
    cvtT G.Int       = Int
    cvtT (G.Arr a b) = Arr (cvtT a) (cvtT b)
