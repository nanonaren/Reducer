module Math.FeatureReduction.Features
    (
      Features
    , diff
    , add
    , union
    , unions
    , delete
    , size
    , member
    , toList
    , fromList
    , split
    , choose2
    , leaveOneOuts
    , toNumber
    , fromNumber
    ) where

import qualified Data.BitSet as B
import Data.List (foldl')
import Data.Bits
import qualified NanoUtils.List as L (chunk,leaveOneOuts)

{- 
******* NOTE **********
The ints must be >= 1
***********************
-}

newtype Features = Features (B.BitSet Int)
    deriving (Show)

instance Eq Features where
    f1 == f2 = toList f1 == toList f2

toNumber :: Features -> Integer
toNumber (Features bs) = B.toIntegral bs

fromNumber :: Integer -> Features
fromNumber num = Features $ B.unsafeFromIntegral num

add :: Int -> Features -> Features
add i fs = union fs (fromList [i])

member :: Int -> Features -> Bool
member i (Features fs) = B.member i fs

split :: Int -> Features -> [Features]
split n = map fromList.L.chunk n.toList

leaveOneOuts :: Features -> [Features]
leaveOneOuts = map fromList.L.leaveOneOuts.toList

choose2 :: [Features] -> [Features]
choose2 = map unions.choose2'

-- |Special choose 2, because if only one elem it will return it
-- I need it to work like this
-- TODO: avoid length
choose2' xs
    | length xs == 1 = [xs]
    | otherwise = choose2'' xs
choose2'' [] = []
choose2'' (x:xs) = [[x,y]|y<-xs] ++ choose2'' xs

unions :: [Features] -> Features
unions = foldl' union (fromList [])

-- |features1 union features2
-- eg: 1010 union 0110 = 1110
union :: Features -> Features -> Features
union (Features f1) (Features f2) =
    Features $ B.unsafeFromIntegral (f1' .|. f2')
    where f1' = B.toIntegral f1 :: Integer
          f2' = B.toIntegral f2 :: Integer

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
