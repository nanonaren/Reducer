module Math.FeatureReduction.Features
    (
      diff
    , toList
    , fromList
    ) where

import qualified Data.BitSet as B
import Data.List (foldl')
import Data.Bits

{- 
******* NOTE **********
The ints must be >= 1
***********************
-}

type Features = B.BitSet Int

-- |features1 - features2
-- eg: 1010 - 0010 = 1000
diff :: Features -> Features -> Features
diff f1 = foldl' (\f i -> B.delete i f) f1.toList

-- |Convert to a list of ints
toList :: Features -> [Int]
toList fs = f 0 (B.toIntegral fs :: Integer)
  where f _ 0 = []
        f n x = if testBit x 0
                then (toEnum n) : f (n+1) (shiftR x 1)
                else f (n+1) (shiftR x 1)

-- |Convert from a list of ints
fromList :: [Int] -> Features
fromList = B.fromList
