{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module MiniAccADT
  where

import Prelude                                  hiding ( exp )
-- import Control.Applicative                      ( (<$>) )
import Data.Typeable
import Text.Printf

import qualified MiniAccGADT                    as GADT


-- Untyped definition
-- ==================

-- Type representation
-- -------------------

data Type
  = TUnit
  | TInt
  | TBool
  | TFloat
  | TTup [Type]
  deriving (Eq, Show)

data Val
  = VUnit
  | VInt Int
  | VBool Bool
  | VFloat Float
  | VTup [Val]
  deriving Eq

instance Show Val where
  show VUnit      = "()"
  show (VInt x)   = show x
  show (VFloat x) = show x
  show (VBool x)  = show x
  show (VTup t)   =
    case t of
      [a,b]   -> show (a,b)
      [a,b,c] -> show (a,b,c)
      _       -> error "VTup: only expected up to 3-tuple"


-- TLM: We could also encode the full type hierarchy, and/or keep the binary
--      tree encoding of tuples.
--
-- data Type         = TZero | TScalar ScalarType | TCons Type Type
--
-- data ScalarType   = NumType | NonNumType
-- data NumType      = IntegralType | FloatingType
-- data IntegralType = TInt
-- data FloatingType = TFloat
-- data NonNumType   = TUnit | TBool


-- Language definition
-- -------------------

type Idx = Int
type ProdIdx = Int

data Exp
  = Let (Type,Exp) (Type,Exp)
  | Var Type Idx                        -- cons indexing!
  | Const Type Val
  | Prj (Type,ProdIdx) (Type,Exp)       -- snoc indexing!
  | Prod Type [Exp]
  | If Type Exp Exp Exp
  | PrimApp PrimFun Exp
  deriving (Eq, Show)

data PrimFun
  = PrimAdd Type
  | PrimMul Type
  | PrimToFloat
  deriving (Eq, Show)


-- Interpreter
-- -----------

type Env = [Val]

eval :: Exp -> Val
eval = evalOpenExp []

evalOpenExp :: Env -> Exp -> Val
evalOpenExp env = go
  where
    go :: Exp -> Val
    go (Let (_,a) (_,b))        = evalOpenExp (go a : env) b
    go (Var _ ix)               = env !! ix             -- assert type! cons indexing!
    go (Const _ c)              = c                     -- assert type!
    go (Prod _ p)               = VTup $ map go p
    go (Prj (_,ix) (_,p))       = prj ix (go p)
    go (If _ p x y)             = if_ (go p) (go x) (go y)
    go (PrimApp f x)            = case f of
                                    PrimAdd t   -> add t (go x)
                                    PrimMul t   -> mul t (go x)
                                    PrimToFloat -> toFloat (go x)

    prj :: ProdIdx -> Val -> Val
    prj ix (VTup vs) = reverse vs !! ix         -- snoc indexing!
    prj _ _          = error "Prj: expected tuple"

    if_ :: Val -> Val -> Val -> Val
    if_ (VBool True)  x _ = x
    if_ (VBool False) _ x = x
    if_ _ _ _             = error "If: incorrect arguments"

    add :: Type -> Val -> Val
    add TInt   (VTup [VInt x,   VInt y])   = VInt   (x+y)
    add TFloat (VTup [VFloat x, VFloat y]) = VFloat (x+y)
    add _ _                                = error "PrimAdd: incorrect arguments"

    mul :: Type -> Val -> Val
    mul TInt   (VTup [VInt x,   VInt y])   = VInt   (x*y)
    mul TFloat (VTup [VFloat x, VFloat y]) = VFloat (x*y)
    mul _ _                                = error "PrimMul: incorrect arguments"

    toFloat :: Val -> Val
    toFloat (VInt x) = VFloat (fromIntegral x)
    toFloat _        = error "PrimToFloat: incorrect arguments"


-- Downing
-- =========
--
-- So long as the untyped expression language has enough constructs to cover all
-- features of the typed language, down always succeeds: it only throws out
-- information, or downgrades it from the type to value level.
--

-- Types

downScalarType :: GADT.ScalarType a -> Type
downScalarType (GADT.NumScalarType t)    = downNumType t
downScalarType (GADT.NonNumScalarType t) = downNonNumScalarType t

downNumType :: GADT.NumType t -> Type
downNumType (GADT.IntegralNumType t) = downIntegralNumType t
downNumType (GADT.FloatingNumType t) = downFloatingNumType t

downIntegralNumType :: GADT.IntegralType t -> Type
downIntegralNumType GADT.TypeInt{} = TInt

downFloatingNumType :: GADT.FloatingType t -> Type
downFloatingNumType GADT.TypeFloat{} = TFloat

downNonNumScalarType :: GADT.NonNumType t -> Type
downNonNumScalarType GADT.TypeUnit   = TUnit
downNonNumScalarType GADT.TypeBool{} = TBool

downTypeR :: GADT.TypeR t -> Type
downTypeR = tup . go
  where
    tup []  = error "empty type!"
    tup [x] = x
    tup xs  = TTup xs

    go :: GADT.TypeR a -> [Type]
    go GADT.TypeRzero                                         = []
    go (GADT.TypeRscalar t)                                   = [ downScalarType t ]
    go (GADT.TypeRsnoc GADT.TypeRzero b@GADT.TypeRsnoc{})     = [ tup (go b) ]
    go (GADT.TypeRsnoc a@GADT.TypeRsnoc{} b@GADT.TypeRsnoc{}) = [ tup (go a), tup (go b) ]
    go (GADT.TypeRsnoc a b)                                   = go a ++ go b

downExpType :: forall env t. GADT.Elt t => GADT.OpenExp env t -> Type
downExpType _ = downTypeR (GADT.eltType (undefined::t))


-- Values

downScalar :: GADT.ScalarType t -> t -> Val
downScalar (GADT.NumScalarType t)    x = downNumScalar t x
downScalar (GADT.NonNumScalarType t) x = downNonNumScalar t x

downNumScalar :: GADT.NumType t -> t -> Val
downNumScalar (GADT.IntegralNumType t) x = downIntegralNum t x
downNumScalar (GADT.FloatingNumType t) x = downFloatingNum t x

downIntegralNum :: GADT.IntegralType t -> t -> Val
downIntegralNum GADT.TypeInt{} = VInt

downFloatingNum :: GADT.FloatingType t -> t -> Val
downFloatingNum GADT.TypeFloat{} = VFloat

downNonNumScalar :: GADT.NonNumType t -> t -> Val
downNonNumScalar GADT.TypeUnit   = const VUnit
downNonNumScalar GADT.TypeBool{} = VBool

downConstR :: GADT.TypeR t -> t -> Val
downConstR ty = tup . go ty
  where
    tup []  = error "empty value!"
    tup [x] = x
    tup xs  = VTup xs

    go :: GADT.TypeR a -> a -> [Val]
    go GADT.TypeRzero                                         ()      = []
    go (GADT.TypeRscalar t)                                   x       = [ downScalar t x ]
    go (GADT.TypeRsnoc GADT.TypeRzero b@GADT.TypeRsnoc{})     ((), x) = [ tup (go b x) ]
    go (GADT.TypeRsnoc a@GADT.TypeRsnoc{} b@GADT.TypeRsnoc{}) (x, y)  = [ tup (go a x), tup (go b y) ]
    go (GADT.TypeRsnoc a b)                                   (x, y)  = go a x ++ go b y


-- Expressions

down :: forall env t. GADT.OpenExp env t -> Exp
down exp =
  case exp of
    GADT.Let a b        -> Let (downExpType a, down a) (downExpType b, down b)
    GADT.Var ix         -> Var (downTypeR (GADT.eltType (undefined::t))) (GADT.idxToInt ix)
    GADT.Const c        -> let t = GADT.eltType (undefined::t)
                           in  Const (downTypeR t) (downConstR t c)
    GADT.Prod p         -> prod (downTypeR (GADT.eltType (undefined::t))) p
    GADT.If p x y       -> If (downExpType x) (down p) (down x) (down y)
    GADT.PrimApp f x    -> PrimApp (prim f) (down x)
    GADT.Prj ix p       -> Prj (downTypeR (GADT.eltType (undefined::t)), GADT.prodIdxToInt ix) (downExpType p, down p)

  where
    prod :: Type -> GADT.Prod (GADT.OpenExp env) p -> Exp
    prod t =
      let go :: GADT.Prod (GADT.OpenExp env) p -> [Exp]
          go GADT.EmptyProd      = []
          go (GADT.PushProd p e) = down e : go p
      in
      Prod t . reverse . go

    prim :: GADT.PrimFun f -> PrimFun
    prim (GADT.PrimAdd t) = PrimAdd (downNumType t)
    prim (GADT.PrimMul t) = PrimMul (downNumType t)
    prim GADT.PrimToFloat = PrimToFloat


-- Up conversion
-- =============
--
-- Up conversion can fail. It takes an untyped expression, or types only at the
-- value level, and attempts to promote type information to the type level.
--
up :: Typeable t => Exp -> GADT.Exp t
up exp = upOpenExp EmptyLayout exp

upOpenExp :: forall env t. Typeable t => Layout env env -> Exp -> GADT.OpenExp env t
upOpenExp lyt = cvt
  where
    -- We could reduce the 'Elt' constraint here to 'Typeable' (which is
    -- required to support 'unify'), and use 'eltType' to extract the class
    -- constraint locally at each constructor.
    --
    cvt :: forall s. Typeable s => Exp -> GADT.OpenExp env s
    cvt (Var t ix)                                      -- type check occurs in upIdx
      | Elt' (_ :: s')  <- elt' t
      , Just Refl       <- unify (undefined::s) (undefined::s')
      = GADT.Var $ upIdx ix lyt

    cvt (Let (ta,a) (tb,b))
      | Elt' (_ :: a)                   <- elt' ta      -- In this case the type of the bound expression is existentially
      , Elt' (_ :: b)                   <- elt' tb      -- quantified, so we must encode its type in the untyped term tree.
      , Just Refl                       <- unify (undefined::s) (undefined::b)
      , a' :: GADT.OpenExp env a        <- cvt a
      , b'                              <- upOpenExp (incLayout lyt `PushLayout` GADT.ZeroIdx) b
      = GADT.Let a' b'

    cvt (Const t c)
      | Elt' (_ :: s')                  <- elt' t                               -- We could assume that 's' is correct, but this
      , Just Refl                       <- unify (undefined::s) (undefined::s') -- way we get a type error message instead of a
      = GADT.Const $ upConstR (GADT.eltType (undefined::s)) c             -- pattern match failure in 'upConst'

    cvt (Prod t p)
      | Just (IsProduct' (_ :: s'))     <- isProduct' t
      , Just Refl                       <- unify (undefined::s) (undefined::s')
      = GADT.Prod (upProd lyt (GADT.prodR (undefined::s)) p)

    cvt (Prj (te,ix) (tp,p))
      | Elt' (_ :: s')                  <- elt' te
      , Just (IsProduct' (_ :: p))      <- isProduct' tp
      , Just Refl                       <- unify (undefined::s) (undefined::s')
      , p' :: GADT.OpenExp env p        <- cvt p                                -- Here we can explicitly require the conversion result to be type 'p'
      , ix'                             <- upProdIdx ix (GADT.prodR (undefined::p))
      = GADT.Prj ix' p'

    cvt (If t p x y)
      | Elt' (_ :: s')                  <- elt' t
      , Just Refl                       <- unify (undefined::s) (undefined::s')
      = GADT.If (cvt p) (cvt x) (cvt y)

    cvt (PrimApp f x)
      | Just (PrimFun' (f' :: GADT.PrimFun (a -> s')))  <- upPrimFun f
      , Just Refl                                       <- unify (undefined::s) (undefined::s')
      , x'                                              <- cvt x
      = GADT.PrimApp f' x'

    cvt _
      = error "up failed"


-- Promoting types
-- ---------------

-- Construct some _sealed_ proof that our value-level types imply class
-- membership.
--
data Elt' where
  Elt' :: GADT.Elt t => {- dummy -} t -> Elt'

data IsProduct' where
  IsProduct' :: (GADT.Elt p, GADT.IsProduct p) => {- dummy -} p -> IsProduct'

elt' :: Type -> Elt'
elt' TUnit   = Elt' ()
elt' TInt    = Elt' (undefined :: Int)
elt' TFloat  = Elt' (undefined :: Float)
elt' TBool   = Elt' (undefined :: Bool)
elt' (TTup t)
  | [ta,tb]       <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  = Elt' (undefined :: (a,b))
  --
  | [ta,tb,tc]    <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  , Elt' (_ :: c) <- elt' tc
  = Elt' (undefined :: (a,b,c))
  --
  | [ta,tb,tc,td] <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  , Elt' (_ :: c) <- elt' tc
  , Elt' (_ :: d) <- elt' td
  = Elt' (undefined :: (a,b,c,d))
  --
  | otherwise
  = error "elt': I only know how to handle up to 4-tuples"


isProduct' :: Type -> Maybe IsProduct'
isProduct' TUnit   = Nothing
isProduct' TInt    = Nothing
isProduct' TFloat  = Nothing
isProduct' TBool   = Nothing
isProduct' (TTup t)
  | [ta,tb]       <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  = Just $ IsProduct' (undefined :: (a,b))
  --
  | [ta,tb,tc]    <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  , Elt' (_ :: c) <- elt' tc
  = Just $ IsProduct' (undefined :: (a,b,c))
  --
  | [ta,tb,tc,td] <- t
  , Elt' (_ :: a) <- elt' ta
  , Elt' (_ :: b) <- elt' tb
  , Elt' (_ :: c) <- elt' tc
  , Elt' (_ :: d) <- elt' td
  = Just $ IsProduct' (undefined :: (a,b,c,d))
  --
  | otherwise
  = error "isProduct': I only know how to handle up to 4-tuples"


-- Utilities
-- ---------

typeError :: forall s t a. (Typeable s, Typeable t) => s -> t -> a
typeError _ _
  = error
  $ printf "Couldn't match expected type `%s' with actual type `%s'"
           (show (typeOf (undefined::s)))
           (show (typeOf (undefined::t)))

inconsistent :: String -> a
inconsistent s = error (s ++ ": inconsistent valuation")

-- Gain some type-level knowledge when two value-level types match
--
unify :: (Typeable s, Typeable t) => s -> t -> Maybe (s :~: t)
unify s t =
  case eqT of
    Nothing   -> typeError s t
    refl      -> refl


-- Indices of let-bound variables
-- ------------------------------

-- Layouts map environments to indices projecting into that environment. The two
-- separate environment types are required because the weakening step
-- (incLayout) happens separately to adding a new term when we push under a
-- binder.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t
              => Layout env env' -> GADT.Idx env t -> Layout env (env',t)

-- Increment all the indices in the layout. This makes space at the head of the
-- layout to add a new index, for when we push under a binder.
--
incLayout :: Layout env env' -> Layout (env, t) env'
incLayout EmptyLayout         = EmptyLayout
incLayout (PushLayout lyt ix) = PushLayout (incLayout lyt) (GADT.SuccIdx ix)

-- Get an index out of the environment
--
upIdx :: forall t env env'. Typeable t => Int -> Layout env env' -> GADT.Idx env t
upIdx 0 (PushLayout _ (ix :: GADT.Idx env t'))
  | Just ix' <- gcast ix          = ix'
  | otherwise                     = typeError (undefined::t) (undefined::t')
upIdx n (PushLayout lyt _)  = upIdx (n-1) lyt
upIdx _ _                   = error "unbound variable"


-- Constant values
-- ---------------

-- TLM: ugh, this is difficult because we threw away some structure when we
--      flattened tuple types into lists analogous to the surface
--      representation, instead of keeping the binary tree encoding; see the
--      final case of 'downConstR' which uses (++).
--
upConstR :: GADT.TypeR t -> Val -> t
upConstR t = go t . untup
  where
    untup :: Val -> [Val]
    untup (VTup vs) = vs
    untup x         = [x]

    go :: GADT.TypeR a -> [Val] -> a
    go GADT.TypeRzero                                           []      = ()
    go (GADT.TypeRscalar s)                                     [v]     = upScalar s v
    go (GADT.TypeRsnoc GADT.TypeRzero b@GADT.TypeRsnoc{})       [v]     = ((), go b (untup v))
    go (GADT.TypeRsnoc GADT.TypeRzero b@GADT.TypeRscalar{})     v       = ((), go b v)
    go (GADT.TypeRsnoc a@GADT.TypeRsnoc{} b@GADT.TypeRscalar{}) xs      = (go a (init xs), go b [last xs])
    go (GADT.TypeRsnoc a b)                                     [x,y]   = (go a (untup x), go b (untup y))
    go _                                                        _       = inconsistent "upConstR"

upScalar :: GADT.ScalarType t -> Val -> t
upScalar (GADT.NumScalarType t)    v = upNumScalar t v
upScalar (GADT.NonNumScalarType t) v = upNonNumScalar t v

upNumScalar :: GADT.NumType t -> Val -> t
upNumScalar (GADT.IntegralNumType t) v = upIntegral t v
upNumScalar (GADT.FloatingNumType t) v = upFloating t v

upIntegral :: GADT.IntegralType t -> Val -> t
upIntegral GADT.TypeInt{} (VInt x) = x
upIntegral _ _ = inconsistent "upIntegral"

upFloating :: GADT.FloatingType t -> Val -> t
upFloating GADT.TypeFloat{} (VFloat x) = x
upFloating _ _ = inconsistent "upFloating"

upNonNumScalar :: GADT.NonNumType t -> Val -> t
upNonNumScalar GADT.TypeUnit   VUnit     = ()
upNonNumScalar GADT.TypeBool{} (VBool x) = x
upNonNumScalar _ _ = inconsistent "upNonNumScalar"


-- Products
-- --------

upProd :: forall env p. Layout env env -> GADT.ProdR p -> [Exp] -> GADT.Prod (GADT.OpenExp env) p
upProd lyt prod = go prod . reverse
  where
    go :: GADT.ProdR s -> [Exp] -> GADT.Prod (GADT.OpenExp env) s
    go GADT.ProdRzero     []     = GADT.EmptyProd
    go (GADT.ProdRsnoc p) (e:es) = GADT.PushProd (go p es) (upOpenExp lyt e)
    go _                  _      = inconsistent "upProd"


upProdIdx :: forall p e. Typeable e => Int -> GADT.ProdR p -> GADT.ProdIdx p e
upProdIdx 0 (GADT.ProdRsnoc _)
  | Just ix <- gcast GADT.ZeroProdIdx   = ix
upProdIdx n (GADT.ProdRsnoc p)    = GADT.SuccProdIdx (upProdIdx (n-1) p)
upProdIdx _ _                     = error "invalid projection"


-- Primitive functions
-- -------------------

data NumType' where
  NumType' :: GADT.Elt a => GADT.NumType a -> NumType'

data PrimFun' where
  PrimFun' :: (GADT.Elt a, GADT.Elt r) => GADT.PrimFun (a -> r) -> PrimFun'

numType' :: Type -> Maybe NumType'
numType' TInt   = Just $ NumType' (GADT.IntegralNumType (GADT.TypeInt   GADT.IntegralDict))
numType' TFloat = Just $ NumType' (GADT.FloatingNumType (GADT.TypeFloat GADT.FloatingDict))
numType' _      = Nothing

upPrimFun :: PrimFun -> Maybe PrimFun'
upPrimFun (PrimAdd t)
  | Just (NumType' dict) <- numType' t
  = Just (PrimFun' (GADT.PrimAdd dict))

upPrimFun (PrimMul t)
  | Just (NumType' dict) <- numType' t
  = Just (PrimFun' (GADT.PrimMul dict))

upPrimFun PrimToFloat
  = Just (PrimFun' GADT.PrimToFloat)

upPrimFun _
  = Nothing
