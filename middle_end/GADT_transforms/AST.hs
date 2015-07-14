{-# LANGUAGE GADTs #-}

-- Language definition
--
module AST where

import Array.Sugar
import Type

-- Environments
-- ------------

data Idx env t where
  ZeroIdx ::              Idx (env, t) t
  SuccIdx :: Idx env t -> Idx (env, s) t

data Env env where
  EmptyEnv ::                 Env ()
  PushEnv  :: Env env -> a -> Env (env, a)

prjIdx :: Idx env t -> Env env -> t
prjIdx ZeroIdx      (PushEnv _ t)   = t
prjIdx (SuccIdx ix) (PushEnv env _) = prjIdx ix env
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

type Acc a = OpenAcc () a

data OpenAcc aenv a where
  Alet          :: OpenAcc aenv        bnd
                -> OpenAcc (aenv, bnd) body
                -> OpenAcc aenv        body

  Avar          :: Idx aenv a
                -> OpenAcc aenv a

  Use           :: (Shape sh, Elt e)
                => Array sh e
                -> OpenAcc aenv (Array sh e)

  Map           :: (Shape sh, Elt a, Elt b)
                => Fun aenv (a -> b)
                -> OpenAcc aenv (Array sh a)
                -> OpenAcc aenv (Array sh b)

  Generate      :: (Shape sh, Elt e)
                => Exp aenv sh
                -> Fun aenv (sh -> e)
                -> OpenAcc aenv (Array sh e)


-- Scalar functions
-- ----------------

type Fun aenv f = OpenFun () aenv f

data OpenFun env aenv f where
  Lam  :: Elt a => OpenFun (env,a) aenv b -> OpenFun env aenv (a -> b)
  Body :: Elt f => OpenExp env aenv f     -> OpenFun env aenv f


-- Scalar expressions
-- ------------------

type Exp aenv t = OpenExp () aenv t

data OpenExp env aenv t where
  Let           :: (Elt bnd, Elt body)
                => OpenExp env        aenv bnd
                -> OpenExp (env, bnd) aenv body
                -> OpenExp env        aenv body

  Var           :: Elt t
                => Idx env t
                -> OpenExp env aenv t

  Const         :: Elt t
                => EltRepr t
                -> OpenExp env aenv t

  Prod          :: (Elt t, IsProduct t)
                => Prod (OpenExp env aenv) (ProdRepr t)
                -> OpenExp env aenv t

  Prj           :: (Elt t, IsProduct t, Elt e)
                => ProdIdx (ProdRepr t) e
                -> OpenExp env aenv t
                -> OpenExp env aenv e

  PrimApp       :: (Elt a, Elt r)
                => PrimFun (a -> r)
                -> OpenExp env aenv a
                -> OpenExp env aenv r

  If            :: Elt t
                => OpenExp env aenv Bool
                -> OpenExp env aenv t
                -> OpenExp env aenv t
                -> OpenExp env aenv t

data PrimFun f where
  PrimAdd       :: NumType a -> PrimFun ((a,a) -> a)
  PrimMul       :: NumType a -> PrimFun ((a,a) -> a)
  PrimToFloat   :: PrimFun (Int -> Float)

