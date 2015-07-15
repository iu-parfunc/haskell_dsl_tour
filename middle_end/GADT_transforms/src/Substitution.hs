{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Simultaneous substitution
--
module Substitution where

import AST
import Array.Sugar


-- | Replace the first variable (ZeroIdx) with the given expression. The
-- environment shrinks.
--
inline
    :: Elt t
    => OpenExp (env, s) aenv t
    -> OpenExp env      aenv s
    -> OpenExp env      aenv t
inline f g = rebuild (subTop g) f
  where
    subTop :: Elt t => OpenExp env aenv s -> Idx (env, s) t -> OpenExp env aenv t
    subTop s ZeroIdx      = s
    subTop _ (SuccIdx ix) = Var ix


-- | Composition of unary functions.
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

compose _ _ = error "impossible evaluation"


-- SEE: [Renaming and Substitution]
-- SEE: [Weakening]
--
class Syntactic f where
  varIn  :: Elt t => Idx env t    -> f env aenv t
  expOut :: Elt t => f env aenv t -> OpenExp env aenv t
  weaken :: Elt t => f env aenv t -> f (env, s) aenv t


-- Wrapper around indices such that Syntactic elements have the same kind.
-- Here, the second environment 'aenv' is ignored.
--
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
    go (Index a ix)  = Index a (go ix)

    goP :: Prod (OpenExp env aenv) p -> Prod (OpenExp env' aenv) p
    goP EmptyProd      = EmptyProd
    goP (PushProd p e) = goP p `PushProd` go e


-- NOTE: [Renaming and Substitution]
--
-- To do things like renaming and substitution, we need some operation on
-- variables that we push structurally through terms, applying to each variable.
-- We have a type preserving but environment changing operation:
--
--   v :: forall t. Idx env t -> f env' aenv t
--
-- The crafty bit is that 'f' might represent variables (for renaming) or terms
-- (for substitutions). The demonic forall, --- which is to say that the
-- quantifier is in a position which gives us obligation, not opportunity ---
-- forces us to respect type: when pattern matching detects the variable we care
-- about, happily we discover that it has the type we must respect. The demon is
-- not so free to mess with us as one might fear at first.
--
-- We then lift this to an operation which traverses terms and rebuild them
-- after applying 'v' to the variables:
--
--   rebuild v :: OpenExp env aenv t -> OpenExp env' aenv t
--
-- The Syntactic class tells us what we need to know about 'f' if we want to be
-- able to rebuild terms. In essence, the crucial functionality is to propagate
-- a class of operations on variables that is closed under shifting.
--

-- NOTE: [Weakening]
--
-- Weakening is something we usually take for granted: every time you learn a
-- new word, old sentences still make sense. If a conclusion is justified by a
-- hypothesis, it is still justified if you add more hypotheses. Similarly, a
-- term remains in scope if you bind more (fresh) variables. Weakening is the
-- operation of shifting things from one scope to a larger scope in which new
-- things have become meaningful, but no old things have vanished.
--
-- When we use a named representation (or HOAS) we get weakening for free. But
-- in the de Bruijn representation weakening takes work: you have to shift all
-- variables references to make room for the new bindings.
--
