{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mapping idiomatic Haskell types onto (unzipped) mini-accelerate Arrays

module Array.Sugar where

import Type
import Array.Data

import Data.Typeable
import qualified Data.Vector.Unboxed            as U


-- Arrays in a unzipped/SOA format
-- ===============================

-- |Multi-dimensional arrays for array processing.
--
-- If device and host memory are separate, arrays will be transferred to the
-- device when necessary (if possible asynchronously and in parallel with other
-- tasks) and cached on the device if sufficient memory is available.
--
data Array sh e where
  Array :: (Shape sh, Elt e)
        => EltRepr sh                 -- extent of dimensions = shape
        -> ArrayData (EltRepr e)      -- array payload
        -> Array sh e

deriving instance Typeable Array

-- |Scalars arrays hold a single element
--
type Scalar e = Array DIM0 e

-- |Vectors are one-dimensional arrays
--
type Vector e = Array DIM1 e

instance Show (Array sh e) where
  show arr@(Array sh _) =
    "Array (" ++ show (toElt sh :: sh) ++ ") " ++ show (toList arr)

toList :: Elt e => Array sh e -> [e]
toList (Array _ adata) = map toElt (U.toList adata)

fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e
fromList sh = Array (fromElt sh) . U.fromList . map fromElt . take (size sh)

infixl 9 !
(!) :: Array sh e -> sh -> e
(!) (Array sh adata) ix = toElt (adata U.! toIndex (toElt sh) ix)

newArray :: (Shape sh, Elt e) => sh -> (sh -> e) -> Array sh e
newArray sh f =
  Array (fromElt sh) $ U.generate (size sh) (fromElt . f . fromIndex sh)

shape :: Shape sh => Array sh e -> sh
shape (Array sh _) = toElt sh


-- Surface and representation types for array elements
-- ===================================================

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


class (ArrayElt (EltRepr a), Typeable (EltRepr a), Show a, Typeable a) => Elt a where
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


class (Elt sh, Elt (Any sh)) => Shape sh where
  size          :: sh -> Int
  toIndex       :: sh -> sh -> Int
  fromIndex     :: sh -> Int -> sh

instance Shape Z where
  size Z        = 1
  toIndex Z Z   = 0
  fromIndex Z _ = Z

instance Shape sh => Shape (sh:.Int) where
  size (sh:.sz)                 = size sh * sz
  toIndex (sh:.sz) (ix:.i)      = toIndex sh ix * sz + i
  fromIndex (sh:.sz) i          = fromIndex sh (i `quot` sz) :. i `rem` sz


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
