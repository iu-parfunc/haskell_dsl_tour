
-- | Storage for array data

module Array.Data where

import Data.Vector.Unboxed              as U

-- mini-accelerate Arrays are just unboxed vectors.
type ArrayData e = U.Vector e

class U.Unbox e => ArrayElt e

instance ArrayElt ()
instance ArrayElt Int
instance ArrayElt Float
instance ArrayElt Bool

instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b)
