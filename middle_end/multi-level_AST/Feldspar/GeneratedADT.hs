{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Feldspar.GeneratedADT
       ( Exp(..)
       , Typ(..)
       , TypeDict(..)
       , upExp, downExp
       ) where
import Prelude hiding (Int, Maybe(..), Bool(..))

-- import Data.Type.Equality

data TyEquality a b where
        Refl :: TyEquality a a

------------------------------------------------------------

data TypeDict a where
        ExistentialDict :: TypeDict any
        ArrowTyDict :: TypeDict a -> TypeDict b -> TypeDict (a -> b)
        IntDict :: TypeDict Int
        Tup2Dict :: TypeDict a -> TypeDict b -> TypeDict (Tup2 a b)
        UnitDict :: TypeDict Unit


data Typ arr where
        Int :: Typ Int
        Arr :: Typ v -> Typ w -> Typ (v -> w)

data Var env ans where
        Zro :: Var (Tup2 e1 a) a
        Suc :: Var e2 x -> Var (Tup2 e2 y) x

data Exp env ans where
        Con :: Int -> Exp e1 Int
        Add :: Exp e2 Int -> Exp e2 Int -> Exp e2 Int
        Var :: Var e4 u -> Exp e4 u
        Abs :: Typ r -> Exp (Tup2 e5 r) s -> Exp e5 (r -> s)
        App :: Exp e6 (x -> y) -> Exp e6 x -> Exp e6 y

data Typ' where
        Int' :: Typ'
        Arr' :: Typ' -> Typ' -> Typ'
    deriving Show

data SealedTyp where
        SealedTyp :: TypeDict arr -> Typ arr -> SealedTyp

data Var' where
        Zro' :: Var'
        Suc' :: Var' -> Var'
    deriving Show

data SealedVar env where
        SealedVar :: TypeDict ans -> Var env ans -> SealedVar env

data Exp' where
        Con' :: Int -> Exp'
        Add' :: Exp' -> Exp' -> Exp'
        Var' :: Var' -> Exp'
        Abs' :: Typ' -> Exp' -> Exp'
        App' :: Exp' -> Exp' -> Exp'
    deriving Show

data SealedExp env where
        SealedExp :: TypeDict ans -> Exp env ans -> SealedExp env

data ArrowTy a b where

data Int where
        One :: Int
        Two :: Int
        Three :: Int
    deriving Show

data Maybe a where
        Just :: a -> Maybe a
        Nothing :: Maybe a
    deriving Show

data Bool where
        True :: Bool
        False :: Bool
    deriving Show

data Unit where
        Unit :: Unit
    deriving Show

data Tup2 a b where
        Tup2 :: a -> b -> Tup2 a b
    deriving Show

eqT :: TypeDict t -> TypeDict u -> Maybe (TyEquality t u)
eqT x y
  = case x of
        Tup2Dict a1 b1 -> case y of
                              Tup2Dict a2 b2 -> case eqT a1 a2 of
                                                    Just Refl -> case eqT b1 b2 of
                                                                     Just Refl -> Just Refl
                                                                     Nothing -> Nothing
                                                    Nothing -> Nothing
                              UnitDict -> Nothing
                              IntDict -> Nothing
                              ArrowTyDict a2 b2 -> Nothing
                              _ -> Nothing
        UnitDict -> case y of
                        Tup2Dict a2 b2 -> Nothing
                        UnitDict -> Just Refl
                        IntDict -> Nothing
                        ArrowTyDict a2 b2 -> Nothing
                        _ -> Nothing
        IntDict -> case y of
                       Tup2Dict a2 b2 -> Nothing
                       UnitDict -> Nothing
                       IntDict -> Just Refl
                       ArrowTyDict a2 b2 -> Nothing
                       _ -> Nothing
        ArrowTyDict a1 b1 -> case y of
                                 Tup2Dict a2 b2 -> Nothing
                                 UnitDict -> Nothing
                                 IntDict -> Nothing
                                 ArrowTyDict a2 b2 -> case eqT a1 a2 of
                                                          Just Refl -> case eqT b1 b2 of
                                                                           Just Refl -> Just Refl
                                                                           Nothing -> Nothing
                                                          Nothing -> Nothing
                                 _ -> Nothing
        _ -> Nothing

downTyp :: forall arr . TypeDict arr -> Typ arr -> Typ'
downTyp arr_dict orig
  = case orig of
        Int -> Int'
        Arr a b -> Arr'
                     (downTyp
                        (let v_dict
                               = case arr_dict of
                                     ArrowTyDict left right -> left
                                     _ -> undefined
                           in v_dict)
                        a)
                     (downTyp
                        (let w_dict
                               = case arr_dict of
                                     ArrowTyDict left right -> right
                                     _ -> undefined
                           in w_dict)
                        b)

upTyp :: Typ' -> SealedTyp
upTyp Int' = let arr_dict = IntDict in SealedTyp arr_dict Int
upTyp (Arr' a b)
  = case upTyp a of
        SealedTyp arr_a'_dict a' -> let v_dict = arr_a'_dict in
                                      case upTyp b of
                                          SealedTyp arr_b'_dict b' -> let w_dict = arr_b'_dict in
                                                                        let arr_dict
                                                                              = ArrowTyDict v_dict
                                                                                  w_dict
                                                                          in
                                                                          SealedTyp arr_dict
                                                                            (Arr a' b')

downVar ::
        forall env ans .
          TypeDict env -> TypeDict ans -> Var env ans -> Var'
downVar env_dict ans_dict orig
  = case orig of
        Zro -> Zro'
        Suc a -> Suc'
                   (downVar
                      (let e2_dict
                             = case env_dict of
                                   Tup2Dict a b -> a
                                   _ -> undefined
                         in e2_dict)
                      (let x_dict = ans_dict in x_dict)
                      a)

upVar :: forall env . TypeDict env -> Var' -> SealedVar env
upVar env_dict lower
  = case lower of
        Zro' -> case env_dict of
                    Tup2Dict e1_dict a_dict -> let ans_dict = a_dict in
                                                 SealedVar ans_dict Zro
                    _ -> undefined
        Suc' a -> case env_dict of
                      Tup2Dict e2_dict y_dict -> case upVar e2_dict a of
                                                     SealedVar ans_a'_dict a' -> let x_dict
                                                                                       = ans_a'_dict
                                                                                   in
                                                                                   let ans_dict
                                                                                         = x_dict
                                                                                     in
                                                                                     SealedVar
                                                                                       ans_dict
                                                                                       (Suc a')
                      _ -> undefined

downExp ::
        forall env ans .
          TypeDict env -> TypeDict ans -> Exp env ans -> Exp'
downExp env_dict ans_dict orig
  = case orig of
        Con a -> Con' a
        Add a b -> Add'
                     (downExp (let e2_dict = env_dict in e2_dict) IntDict a)
                     (downExp (let e2_dict = env_dict in e2_dict) IntDict b)
        Var a -> Var'
                   (downVar (let e4_dict = env_dict in e4_dict)
                      (let u_dict = ans_dict in u_dict)
                      a)
        Abs a b -> Abs'
                     (downTyp
                        (let r_dict
                               = case ans_dict of
                                     ArrowTyDict left right -> left
                                     _ -> undefined
                           in r_dict)
                        a)
                     (downExp
                        (let e5_dict = env_dict in
                           let r_dict
                                 = case ans_dict of
                                       ArrowTyDict left right -> left
                                       _ -> undefined
                             in Tup2Dict e5_dict r_dict)
                        (let s_dict
                               = case ans_dict of
                                     ArrowTyDict left right -> right
                                     _ -> undefined
                           in s_dict)
                        b)
        App a b -> App'
                     (downExp (let e6_dict = env_dict in e6_dict)
                        (let x_dict = ExistentialDict in
                           let y_dict = ans_dict in ArrowTyDict x_dict y_dict)
                        a)
                     (downExp (let e6_dict = env_dict in e6_dict)
                        (let x_dict = ExistentialDict in x_dict)
                        b)

upExp :: forall env . TypeDict env -> Exp' -> SealedExp env
upExp env_dict lower
  = case lower of
        Con' a -> let e1_dict = env_dict in
                    let ans_dict = IntDict in SealedExp ans_dict (Con a)
        Add' a b -> let e2_dict = env_dict in
                      case upExp env_dict a of
                          SealedExp ans_a'_dict a' -> case ans_a'_dict of
                                                          IntDict -> case upExp env_dict b of
                                                                         SealedExp ans_b'_dict
                                                                           b' -> case ans_b'_dict of
                                                                                     IntDict -> let ans_dict
                                                                                                      = IntDict
                                                                                                  in
                                                                                                  SealedExp
                                                                                                    ans_dict
                                                                                                    (Add
                                                                                                       a'
                                                                                                       b')
                                                                                     _ -> undefined
                                                          _ -> undefined
        Var' a -> let e4_dict = env_dict in
                    case upVar env_dict a of
                        SealedVar ans_a'_dict a' -> let u_dict = ans_a'_dict in
                                                      let ans_dict = u_dict in
                                                        SealedExp ans_dict (Var a')
        Abs' a b -> let e5_dict = env_dict in
                      case upTyp a of
                          SealedTyp arr_a'_dict a' -> let r_dict = arr_a'_dict in
                                                        case
                                                          upExp
                                                            (let r_dict = arr_a'_dict in
                                                               Tup2Dict e5_dict r_dict)
                                                            b
                                                          of
                                                            SealedExp ans_b'_dict b' -> let s_dict
                                                                                              = ans_b'_dict
                                                                                          in
                                                                                          let ans_dict
                                                                                                = ArrowTyDict
                                                                                                    r_dict
                                                                                                    s_dict
                                                                                            in
                                                                                            SealedExp
                                                                                              ans_dict
                                                                                              (Abs
                                                                                                 a'
                                                                                                 b')
        App' a b -> let e6_dict = env_dict in
                      case upExp env_dict a of
                          SealedExp ans_a'_dict a' -> case ans_a'_dict of
                                                          ArrowTyDict x_dict y_dict -> case
                                                                                         upExp
                                                                                           env_dict
                                                                                           b
                                                                                         of
                                                                                           SealedExp
                                                                                             ans_b'_dict
                                                                                             b' -> case
                                                                                                     eqT
                                                                                                       ans_b'_dict
                                                                                                       x_dict
                                                                                                     of
                                                                                                       Just
                                                                                                         Refl -> let ans_dict
                                                                                                                       = y_dict
                                                                                                                   in
                                                                                                                   SealedExp
                                                                                                                     ans_dict
                                                                                                                     (App
                                                                                                                        a'
                                                                                                                        b')
                                                                                                       Nothing -> undefined
                                                          _ -> undefined

ghostbuster :: Exp Unit Int
ghostbuster
  = case upExp UnitDict (downExp UnitDict IntDict (Con Three)) of
        SealedExp dict x -> case eqT dict IntDict of
                                Just Refl -> x
                                Nothing -> undefined

main :: IO ()
main = seq ghostbuster (return ())
