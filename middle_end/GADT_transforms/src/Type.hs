{-# LANGUAGE GADTs #-}

-- | Type hierarchy
--
module Type where

import Text.PrettyPrint.Leijen


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


-- Type refication
-- ---------------

data TypeR a where
  TypeRzero      ::                       TypeR ()
  TypeRscalar    :: ScalarType a ->       TypeR a
  TypeRsnoc      :: TypeR a -> TypeR b -> TypeR (a, b)

instance Show (TypeR a) where
  show = show . ppTypeR

ppTypeR :: TypeR a -> Doc
ppTypeR = tupled . go
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
