{-# LANGUAGE GADTs #-}

-- Language definition
--
module AST where

import Sugar
import Type

-- Environments
-- ------------

data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

data Env env where
  EmptyEnv ::                 Env ()
  PushEnv  :: a -> Env env -> Env (env, a)

prjIdx :: Idx env t -> Env env -> t
prjIdx ZeroIdx      (PushEnv t _)   = t
prjIdx (SuccIdx ix) (PushEnv _ env) = prjIdx ix env
prjIdx _            _               = error "impossible case"

idxToInt :: Idx env t -> Int
idxToInt ZeroIdx      = 0
idxToInt (SuccIdx ix) = 1 + idxToInt ix

-- Product types
-- -------------

data Prod c p where
  EmptyProd ::                             Prod c ()
  PushProd  :: Elt e => Prod c p -> c e -> Prod c (p, e)

data ProdIdx p e where
  ZeroProdIdx ::                ProdIdx (p, s) s
  SuccProdIdx :: ProdIdx p e -> ProdIdx (p, s) e

prodIdxToInt :: ProdIdx p e -> Int
prodIdxToInt ZeroProdIdx      = 0
prodIdxToInt (SuccProdIdx ix) = 1 + prodIdxToInt ix


-- Array computations
-- ------------------



-- Scalar expressions
-- ------------------

type Exp t = OpenExp () t

data OpenExp env t where
  Let           :: (Elt bnd, Elt body)
                => OpenExp env        bnd
                -> OpenExp (env, bnd) body
                -> OpenExp env        body

  Var           :: Elt t
                => Idx env t
                -> OpenExp env t

  Const         :: Elt t
                => EltRepr t
                -> OpenExp env t

  Prod          :: (Elt t, IsProduct t)
                => Prod (OpenExp env) (ProdRepr t)
                -> OpenExp env t

  Prj           :: (Elt t, IsProduct t, Elt e)
                => ProdIdx (ProdRepr t) e
                -> OpenExp env t
                -> OpenExp env e

  PrimApp       :: (Elt a, Elt r)
                => PrimFun (a -> r)
                -> OpenExp env a
                -> OpenExp env r

  If            :: Elt t
                => OpenExp env Bool
                -> OpenExp env t
                -> OpenExp env t
                -> OpenExp env t

data PrimFun f where
  PrimAdd       :: NumType a -> PrimFun ((a,a) -> a)
  PrimMul       :: NumType a -> PrimFun ((a,a) -> a)
  PrimToFloat   :: PrimFun (Int -> Float)

