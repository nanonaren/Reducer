module Math.FeatureReduction.Features
    (
      Features
    , diff
    , delete
    , size
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

newtype Features = Features (B.BitSet Int)
    deriving (Eq,Show)

-- |features1 - features2
-- eg: 1010 - 0010 = 1000
diff :: Features -> Features -> Features
diff f1 = foldl' delete f1.toList

-- |Remove a feature
delete :: Features -> Int -> Features
delete (Features fs) i = Features $ B.delete i fs

-- |Number of 1s
size :: Features -> Int
size (Features fs) = B.size fs

-- |Convert to a list of ints
toList :: Features -> [Int]
toList (Features fs) = f 0 (B.toIntegral fs :: Integer)
  where f _ 0 = []
        f n x = if testBit x 0
                then (toEnum n) : f (n+1) (shiftR x 1)
                else f (n+1) (shiftR x 1)

-- |Convert from a list of ints
fromList :: [Int] -> Features
fromList xs = Features $ B.fromList xs
