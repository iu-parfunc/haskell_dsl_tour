{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MiniAccGADT
  where

import Data.Typeable
import Text.PrettyPrint.Leijen


-- Product types
-- =============

data ProdR p where
  ProdRzero ::                     ProdR ()
  ProdRsnoc :: Elt e => ProdR p -> ProdR (p, e)

class IsProduct t where
  type ProdRepr t
  fromProd :: t -> ProdRepr t
  toProd   :: ProdRepr t -> t
  prodR    :: {- dummy -} t -> ProdR (ProdRepr t)

instance IsProduct () where
  type ProdRepr () = ()
  fromProd () = ()
  toProd ()   = ()
  prodR _     = ProdRzero

instance (Elt a, Elt b) => IsProduct (a, b) where
  type ProdRepr (a, b) = (((), a), b)
  fromProd (a, b)     = (((), a), b)
  toProd (((), a), b) = (a, b)
  prodR _             = ProdRsnoc (ProdRsnoc ProdRzero)

instance (Elt a, Elt b, Elt c) => IsProduct (a, b, c) where
  type ProdRepr (a, b, c) = (ProdRepr (a, b), c)
  fromProd (a, b, c) = (fromProd (a,b), c)
  toProd (ab, c)     = let (a, b) = toProd ab in (a, b, c)
  prodR _            = ProdRsnoc (prodR (undefined :: (a,b)))

instance (Elt a, Elt b, Elt c, Elt d) => IsProduct (a, b, c, d) where
  type ProdRepr (a, b, c, d) = (ProdRepr (a, b, c), d)
  fromProd (a, b, c, d) = (fromProd (a,b,c), d)
  toProd (abc, d)       = let (a, b, c) = toProd abc in (a, b, c, d)
  prodR _               = ProdRsnoc (prodR (undefined :: (a,b,c)))


-- Type hierarchy
-- ==============

integralDict :: IntegralType a -> IntegralDict a
integralDict (TypeInt d) = d

floatingDict :: FloatingType a -> FloatingDict a
floatingDict (TypeFloat d) = d

data IntegralDict a where
  IntegralDict :: (Integral a, Num a, Show a, Eq a) => IntegralDict a

data FloatingDict a where
  FloatingDict :: (Floating a, Fractional a, Num a, Show a, Eq a) => FloatingDict a

data NonNumDict a where
  NonNumDict   :: (Show a, Eq a) => NonNumDict a

data ScalarType a where
  NumScalarType    :: NumType a -> ScalarType a
  NonNumScalarType :: NonNumType a -> ScalarType a

data NumType a where
  IntegralNumType :: IntegralType a -> NumType a
  FloatingNumType :: FloatingType a -> NumType a

data IntegralType a where
  TypeInt :: IntegralDict Int -> IntegralType Int

data FloatingType a where
  TypeFloat :: FloatingDict Float -> FloatingType Float

data NonNumType a where
  TypeUnit ::                    NonNumType ()
  TypeBool :: NonNumDict Bool -> NonNumType Bool

instance Show (ScalarType t) where
  show (NumScalarType t)    = show t
  show (NonNumScalarType t) = show t

instance Show (NumType a) where
  show (IntegralNumType t) = show t
  show (FloatingNumType t) = show t

instance Show (IntegralType a) where
  show TypeInt{} = "Int"

instance Show (FloatingType a) where
  show TypeFloat{} = "Float"

instance Show (NonNumType a) where
  show TypeUnit   = "()"
  show TypeBool{} = "Bool"


-- Surface and representation types
-- ================================

-- The representation types are _closed_. We convert everything into this
-- format, and the untyped version will work in this format (hopefully).
--
type family EltRepr a where
  EltRepr ()           = ()
  EltRepr Int          = Int
  EltRepr Bool         = Bool
  EltRepr Float        = Float
  EltRepr (a, b)       = ProdRepr (EltRepr a, EltRepr b)
  EltRepr (a, b, c)    = ProdRepr (EltRepr a, EltRepr b, EltRepr c)
  EltRepr (a, b, c, d) = ProdRepr (EltRepr a, EltRepr b, EltRepr c, EltRepr d)

  -- Bonus: shapes introduce non-injectivity into the representation types
  EltRepr Z               = ()
  EltRepr (t:.h)          = (EltRepr t, EltRepr h)
  EltRepr All             = ()
  EltRepr (Any Z)         = ()
  EltRepr (Any (sh:.Int)) = (EltRepr (Any sh), ())


class (Show a, Typeable a, Typeable (EltRepr a)) => Elt a where
  toElt   :: EltRepr a -> a
  fromElt :: a -> EltRepr a
  eltType :: {- dummy -} a -> TypeR (EltRepr a)

instance Elt () where
  toElt     = id
  fromElt   = id
  eltType _ = TypeRscalar (NonNumScalarType TypeUnit)

instance Elt Int where
  toElt     = id
  fromElt   = id
  eltType _ = TypeRscalar (NumScalarType (IntegralNumType (TypeInt IntegralDict)))

instance Elt Float where
  toElt     = id
  fromElt   = id
  eltType _ = TypeRscalar (NumScalarType (FloatingNumType (TypeFloat FloatingDict)))

instance Elt Bool where
  toElt     = id
  fromElt   = id
  eltType _ = TypeRscalar (NonNumScalarType (TypeBool NonNumDict))

instance (Elt a, Elt b) => Elt (a, b) where
  toElt (((),a),b) = (toElt a, toElt b)
  fromElt (a, b)   = (((),fromElt a), fromElt b)
  eltType _        = TypeRzero `TypeRsnoc` eltType (undefined :: a)
                               `TypeRsnoc` eltType (undefined :: b)

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  toElt ((((),a),b),c) = (toElt a, toElt b, toElt c)
  fromElt (a, b, c)    = ((((),fromElt a), fromElt b), fromElt c)
  eltType _            = TypeRzero `TypeRsnoc` eltType (undefined :: a)
                                   `TypeRsnoc` eltType (undefined :: b)
                                   `TypeRsnoc` eltType (undefined :: c)

instance (Elt a, Elt b, Elt c, Elt d) => Elt (a, b, c, d) where
  toElt (((((),a),b),c),d) = (toElt a, toElt b, toElt c, toElt d)
  fromElt (a, b, c, d)     = (((((),fromElt a), fromElt b), fromElt c), fromElt d)
  eltType _                = TypeRzero `TypeRsnoc` eltType (undefined :: a)
                                       `TypeRsnoc` eltType (undefined :: b)
                                       `TypeRsnoc` eltType (undefined :: c)
                                       `TypeRsnoc` eltType (undefined :: d)

-- Bonus: shapes
-- -------------

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
type DIM3 = DIM2 :. Int

data Z = Z
  deriving (Show, Typeable)

infixl 3 :.
data tail :. head = tail :. head
  deriving (Show, Typeable)

data All = All
  deriving (Show, Typeable)

data Any sh = Any
  deriving (Show, Typeable)

instance Elt Z where
  toElt ()  = Z
  fromElt Z = ()
--  eltType _ = TypeRscalar (NonNumScalarType TypeUnit)
  eltType _ = TypeRzero         -- doesn't need a value-level representation (??)

instance Elt All where
  toElt ()    = All
  fromElt All = ()
  eltType _   = TypeRscalar (NonNumScalarType TypeUnit)

instance Elt (Any Z) where
  toElt _   = Any
  fromElt _ = ()
  eltType _ = TypeRzero         -- doesn't need a value-level representation (??)

instance Shape sh => Elt (Any (sh:.Int)) where
  toElt _   = Any
  fromElt _ = (fromElt (undefined :: Any sh), ())
  eltType _ = eltType (undefined :: (Any sh)) `TypeRsnoc` TypeRscalar (NonNumScalarType TypeUnit)

instance (Elt t, Elt h) => Elt (t:.h) where
  toElt (t,h)    = toElt t :. toElt h
  fromElt (t:.h) = (fromElt t, fromElt h)
  eltType _      = eltType (undefined :: t) `TypeRsnoc` eltType (undefined :: h)


class (Elt sh, Elt (Any sh)) => Shape sh

instance Shape Z
instance Shape sh => Shape (sh:.Int)


-- Type refication
-- ---------------

data TypeR a where
  TypeRzero      ::                       TypeR ()
  TypeRscalar    :: ScalarType a ->       TypeR a
  TypeRsnoc      :: TypeR a -> TypeR b -> TypeR (a, b)

instance Show (TypeR a) where
  show = show . ppTypeR'

-- ppTypeR :: TypeR a -> Doc
-- ppTypeR TypeRzero       = text "TypeRzero"
-- ppTypeR (TypeRsnoc a b) = ppTypeR a <+> text "`TypeRsnoc`" <+> ppTypeR b
-- ppTypeR (TypeRscalar t) = text (show t)

ppTypeR' :: TypeR a -> Doc
ppTypeR' = tupled . go
  where
    tup []  = empty
    tup [x] = x
    tup xs  = tupled xs

    go :: TypeR a -> [Doc]
    go TypeRzero                                = []
    go (TypeRscalar t)                          = [ text (show t) ]
    go (TypeRsnoc TypeRzero     b@TypeRsnoc{})  = [ tup (go b) ]                -- 'b' is terminated at this point
    go (TypeRsnoc a@TypeRsnoc{} b@TypeRsnoc{})  = [ tup (go a), tup (go b) ]    -- meet point of a pair of tuples
    go (TypeRsnoc a b)                          = go a ++ go b

t1 = eltType (undefined :: ((Bool,Int), Float))
t2 = eltType (undefined :: (Bool,Int,Float))
t3 = eltType (undefined :: (Bool, (Int,Float)))
t4 = eltType (undefined :: (Int,Int,Int,Float))
t5 = eltType (undefined :: (Bool, (Int,Float), Int))
t6 = eltType (undefined :: ((Int,Int),(Int,Float)))
t7 = eltType (undefined :: (((Int,Int),(Int,Float)),Float))

t8 = eltType (undefined :: DIM1)
t9 = eltType (undefined :: DIM2)
t10 = eltType (undefined :: Any (DIM2))
t11 = eltType (undefined :: Z :. Int :. All)
t12 = eltType (undefined :: Z :. All :. Int)


-- Language definition
-- ===================

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


-- Language definition
-- -------------------

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


-- Pretty printing
-- ---------------

prettyExp :: forall env t. Int -> (Doc -> Doc) -> OpenExp env t -> Doc
prettyExp lvl wrap = pp
  where
    ppE :: OpenExp env s -> Doc
    ppE = prettyExp lvl parens

    pp :: OpenExp env t -> Doc
    pp (Var ix)         = char 'x' <> int (lvl - idxToInt ix - 1)
    pp (Const c)        = text (show (toElt c :: t))
    pp (Prj ix tup)     = wrap (char '#' <> int (prodIdxToInt ix) <+> ppE tup)

    pp (Let a b) =
      let x  = char 'x' <> int lvl
          a' = prettyExp lvl     id a
          b' = prettyExp (lvl+1) id b

          isLet Let{} = True
          isLet _     = False
      in
      if not (isLet a) && isLet b
         then vcat [ text "let" <+> x <+> equals <+> a' <+> text "in", b'               ]
         else vcat [ text "let" <+> x <+> equals <+> a',               text "in" <+> b' ]

    pp (Prod p) =
      let collect :: Prod (OpenExp env) p -> [Doc]
          collect EmptyProd      = []
          collect (PushProd t e) = prettyExp lvl id e : collect t
      in
      tupled (reverse (collect p))

    pp (PrimApp f x) =
      let ppF :: PrimFun f -> Doc
          ppF PrimAdd{}     = text "(+)"
          ppF PrimMul{}     = text "(*)"
          ppF PrimToFloat{} = text "toFloat"
      in
      wrap (ppF f <+> ppE x)

    pp (If p t e) =
      hang 3 $ vcat [ text "if"   <+> ppE p, text "then" <+> ppE t, text "else" <+> ppE e ]


instance Show (OpenExp env t) where
  show = show . prettyExp 0 id


-- Interpreter
-- -----------

eval :: Exp t -> t
eval = evalOpenExp EmptyEnv

evalOpenExp :: forall env t. Env env -> OpenExp env t -> t
evalOpenExp env = go
  where
    go :: OpenExp env s -> s
    go (Let a b)        = evalOpenExp (PushEnv (go a) env) b
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

    prod :: Prod (OpenExp env) p -> p
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


-- Tests
-- =====

constant :: Elt t => t -> OpenExp env t
constant = Const . fromElt

p0 :: Exp Int
p0 = If (constant True) (constant 3) (constant 4)

p1 :: Exp Int
p1 = Let (constant 5) (Var ZeroIdx)

p2 :: Exp Int
p2 = If (constant True) (constant 11) p1

p3 :: Exp Int
p3 = Let (constant 5) (If (constant True) (Var ZeroIdx) (constant 4))

p4 :: Exp Int
p4 = Let (constant 4)
   $ Let (constant 5)
   $ PrimApp (PrimAdd (IntegralNumType (TypeInt IntegralDict)))
             (Prod $ EmptyProd `PushProd` Var ZeroIdx
                               `PushProd` Var (SuccIdx ZeroIdx))

p5 :: Exp Bool
p5 = constant True

p6 :: Exp Bool
p6 = Let p5
   $ If (Var ZeroIdx) (constant False)
                      (Var ZeroIdx)

p7 :: Exp (Int,Float)
p7 = constant (1,pi)

p8 :: Exp Float
p8 = Let p7
   $ Prj ZeroProdIdx (Var ZeroIdx)

p9 :: Exp Float
p9 = Let (constant (pi, 8, 4.86))
   $ Let (PrimApp (PrimMul (FloatingNumType (TypeFloat FloatingDict)))
                  (Prod (EmptyProd `PushProd` Prj ZeroProdIdx (Var ZeroIdx)
                                   `PushProd` PrimApp PrimToFloat (Prj (SuccProdIdx ZeroProdIdx) (Var ZeroIdx)))))
         (PrimApp (PrimAdd (FloatingNumType (TypeFloat FloatingDict)))
                  (Prod (EmptyProd `PushProd` Var ZeroIdx
                                   `PushProd` Prj (SuccProdIdx (SuccProdIdx ZeroProdIdx)) (Var (SuccIdx ZeroIdx)))))

p10 :: Exp (Int, (Float, Int), Bool)
p10 = constant (1, (4,2), True)

p11 :: Exp (Float,Int)
p11 = Let p10
    $ Prj (SuccProdIdx ZeroProdIdx) (Var ZeroIdx)

p12 :: Exp DIM1
p12 = constant (Z :. 10)

p13 :: Exp (Any DIM2)
p13 = constant Any

p14 :: Exp ((Bool,Int), Float)
p14 = constant ((True,42), pi)

p15 :: Exp (Bool,Int,Float)
p15 = constant (True,1,2)

p16 :: Exp (Bool, (Int,Float))
p16 = constant (False, (12,42))

p17 :: Exp Float
p17 = Let p3
    $ PrimApp PrimToFloat (Var ZeroIdx)

p18 :: Exp DIM1
p18 = Let (constant (4 :: Int, Z:.10 :: DIM1))
    $ Prj ZeroProdIdx (Var ZeroIdx)
