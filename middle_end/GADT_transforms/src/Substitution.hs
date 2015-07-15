{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Simultaneous substitution
--
module Substitution where

import AST
import Array.Sugar


-- Replace the first variable (ZeroIdx) with the given expression. The
-- environment shrinks.
--
substitute
    :: Elt t
    => OpenExp (env, s) aenv t
    -> OpenExp env      aenv s
    -> OpenExp env      aenv t
substitute f g = rebuild (subTop g) f
  where
    subTop :: Elt t => OpenExp env aenv s -> Idx (env, s) t -> OpenExp env aenv t
    subTop s ZeroIdx      = s
    subTop _ (SuccIdx ix) = Var ix


-- Composition of unary functions.
--
compose
    :: Elt c
    => OpenFun env aenv (b -> c)
    -> OpenFun env aenv (a -> b)
    -> OpenFun env aenv (a -> c)
compose (Lam (Body f)) (Lam (Body g)) = Lam . Body $ rebuild (dot g) f
  where
    dot :: Elt c => OpenExp (env, a) aenv b -> Idx (env, b) c -> OpenExp (env, a) aenv c
    dot s ZeroIdx      = s
    dot _ (SuccIdx ix) = Var (SuccIdx ix)
compose _              _              = error "impossible evaluation"


class Syntactic f where
  varIn  :: Elt t => Idx env t    -> f env env' t
  expOut :: Elt t => f env env' t -> OpenExp env env' t
  weaken :: Elt t => f env env' t -> f (env, s) env' t


newtype Idx' env aenv t = I { unI :: Idx env t }

instance Syntactic Idx' where
  varIn  = I
  expOut = Var . unI
  weaken = I . SuccIdx . unI

instance Syntactic OpenExp where
  varIn  = Var
  expOut = id
  weaken = rebuild (weaken . I)


shift :: (Syntactic f, Elt t)
      => (forall t'. Elt t' => Idx env t' -> f env' aenv t')
      -> Idx' (env,  s) aenv t
      -> f    (env', s) aenv t
shift _ (I ZeroIdx)      = varIn ZeroIdx
shift v (I (SuccIdx ix)) = weaken (v ix)


rebuild :: forall env env' aenv f t. Syntactic f
        => (forall t'. Elt t' => Idx env t' -> f env' aenv t')
        -> OpenExp env  aenv t
        -> OpenExp env' aenv t
rebuild v = go
  where
    go :: OpenExp env aenv s -> OpenExp env' aenv s
    go (Let a b)     = Let (go a) (rebuild (shift v . I) b)
    go (Var ix)      = expOut (v ix)
    go (Const c)     = Const c
    go (PrimApp f x) = PrimApp f (go x)
    go (If p t e)    = If (go p) (go t) (go e)
    go (Prj ix p)    = Prj ix (go p)
    go (Prod p)      = Prod (goP p)

    goP :: Prod (OpenExp env aenv) p -> Prod (OpenExp env' aenv) p
    goP EmptyProd      = EmptyProd
    goP (PushProd p e) = goP p `PushProd` go e

