{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- Surface and representation types
--
module Sugar where

import Type
import Data.Typeable


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

