{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Mini feldspar GADT, copied from:
--     https://github.com/shayan-najd/MiniFeldspar/

module Feldspar.GADT where

import Data.Typeable (Typeable)
import Text.PrettyPrint.Leijen

-- GADT representation.
-- Simply-typed lambda calculus with de Bruijn indices,
-- with integer constants, and addition.
-- Philip Wadler and Shayan Najd, November 2013

-- Variables
type role Var nominal nominal
data Var e a where
  Zro :: Var (e,a) a  -- This requires role nominal for the environment param.
  Suc :: Var e a -> Var (e,b) a -- So does this
 deriving Typeable

type role Exp nominal nominal
data Exp (e :: *) (a :: *) where
  Con :: Int -> Exp e Int
  Add :: Exp e Int -> Exp e Int -> Exp e Int
  Mul :: Exp e Int -> Exp e Int -> Exp e Int
  Var :: Var e a -> Exp e a
  Abs :: Typ a -> Exp (e,a) b -> Exp e (a -> b)
  App :: Exp e (a -> b) -> Exp e a -> Exp e b
 deriving Typeable

-- Types (Singleton)
data Typ a where
  Int :: Typ Int
  Arr :: Typ a -> Typ b -> Typ (a -> b)
 deriving Typeable

-- Environment (Singleton)
data Env e where
  Emp :: Env ()
  Ext :: Env e -> Typ a -> Env (e,a)
 deriving Typeable

class Elt a where
  eltType :: Typ a

instance Elt Int where
  eltType = Int

instance (Elt a, Elt b) => Elt (a -> b) where
  eltType = Arr eltType eltType

instance Num (Exp env Int) where
  (+) = Add
  (*) = Mul
  fromInteger n = Con (fromInteger n)

  --
  (-)           = error "Exp.(-)"
  abs           = error "Exp.abs"
  signum        = error "Exp.signum"

-- Helpers
-- -------

let_ :: Elt a => Exp env a -> Exp (env,a) b -> Exp env b
let_ bnd body = (Abs eltType body) `App` bnd

constant :: Int -> Exp env Int
constant = Con


-- Pretty printer
-- --------------

idxToInt :: Var env t -> Int
idxToInt Zro      = 0
idxToInt (Suc ix) = idxToInt ix + 1

prettyOpenExp :: (Doc -> Doc) -> Int -> Exp env a -> Doc
prettyOpenExp wrap lvl = pp
  where
    ppE :: Exp env a -> Doc
    ppE = prettyOpenExp parens lvl

    ppF :: Exp env a -> Doc
    ppF fun =
      let
          (n, body) = count n fun

          count :: Int -> Exp env f -> (Int, Doc)
          count l (Abs _ f) = let (i,b) = count l f in (i+1, b)
          count l other     = (lvl-1, prettyOpenExp id (l+1) other)
      in
      parens $ sep [ char 'λ' <> hsep [ char 'x' <> int idx | idx <- [lvl .. n] ] <+> char '→'
                   , hang 2 body ]

    pp :: Exp env a -> Doc
    pp (Con i)          = int i
    pp (Var ix)         = char 'x' <> int (lvl - idxToInt ix - 1)
    pp (Add x y)        = wrap $ ppE x <+> char '+' <+> ppE y
    pp (Mul x y)        = wrap $ ppE x <+> char '*' <+> ppE y
    pp (App f x)        = wrap $ sep [ ppE f, hang 2 (ppE x) ]
    pp f@Abs{}          = ppF f


prettyType :: Typ a -> Doc
prettyType Int       = text "Int"
prettyType (Arr a b) = parens (prettyType a <+> char '→' <+> prettyType b)

instance Show (Exp env a) where
  show = show . prettyOpenExp id 0

instance Show (Typ a) where
  show = show . prettyType
