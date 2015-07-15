{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import AST
import Type
import Array.Sugar

run :: Acc a -> a
run = evalOpenAcc EmptyEnv


evalOpenAcc :: forall aenv a. Env aenv -> OpenAcc aenv a -> a
evalOpenAcc aenv = go
  where
    goF :: Fun aenv f -> f
    goF = evalOpenFun EmptyEnv aenv

    goE :: Exp aenv e -> e
    goE = evalOpenExp EmptyEnv aenv

    go :: OpenAcc aenv s -> s
    go (Alet a b)       = evalOpenAcc (aenv `PushEnv` go a) b
    go (Avar ix)        = prjIdx ix aenv
    go (Use xs)         = xs
    go (Map f xs)       = mapOp (goF f) (go xs)
    go (Generate sh f)  = newArray (goE sh) (goF f)


mapOp :: (Shape sh, Elt a, Elt b)
      => (a -> b)
      -> Array sh a
      -> Array sh b
mapOp f xs = newArray (shape xs) (\ix -> f (xs ! ix))


evalOpenFun :: Env env -> Env aenv -> OpenFun env aenv f -> f
evalOpenFun env aenv (Body e) = evalOpenExp env aenv e
evalOpenFun env aenv (Lam f)  = \x -> evalOpenFun (env `PushEnv` x) aenv f


evalOpenExp :: forall env aenv t. Env env -> Env aenv -> OpenExp env aenv t -> t
evalOpenExp env aenv = go
  where
    go :: OpenExp env aenv s -> s
    go (Let a b)        = evalOpenExp (env `PushEnv` go a) aenv b
    go (Var ix)         = prjIdx ix env
    go (Const c)        = toElt c
    go (Prj ix p)       = prj ix (fromProd (go p))
    go (Prod p)         = toProd (prod p)
    go (PrimApp f x)    = prim f (go x)
    go (If p a b)
      | go p            = go a
      | otherwise       = go b

    prj :: ProdIdx p e -> p -> e
    prj ZeroProdIdx      (_, e) = e
    prj (SuccProdIdx ix) (p, _) = prj ix p

    prod :: Prod (OpenExp env aenv) p -> p
    prod EmptyProd      = ()
    prod (PushProd p v) = (prod p, go v)

    prim :: PrimFun f -> f
    prim (PrimAdd t) = add t
    prim (PrimMul t) = mul t
    prim PrimToFloat = fromIntegral

    add :: NumType a -> ((a,a) -> a)
    add (IntegralNumType t) | IntegralDict <- integralDict t = uncurry (+)
    add (FloatingNumType t) | FloatingDict <- floatingDict t = uncurry (+)

    mul :: NumType a -> ((a,a) -> a)
    mul (IntegralNumType t) | IntegralDict <- integralDict t = uncurry (*)
    mul (FloatingNumType t) | FloatingDict <- floatingDict t = uncurry (*)

