
-- | Storage for array data
--
module Array.Data where

import Data.Vector.Unboxed              as U

-- Mini-Accelerate Arrays are just unboxed vectors. We only need vectors of
-- primitive types (Int, Float, etc.) and do the unzipping of compound
-- types ourselves.
--
type ArrayData e = U.Vector e

class U.Unbox e => ArrayElt e

instance ArrayElt ()
instance ArrayElt Int
instance ArrayElt Float
instance ArrayElt Bool
instance (ArrayElt a, ArrayElt b) => ArrayElt (a, b)

