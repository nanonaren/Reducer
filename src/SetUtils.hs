{-# LANGUAGE TupleSections #-}
module SetUtils
    (
      randCoprimeFactors
    ) where

import Data.Int
import Data.List (partition,find)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.HashTable (hashString)
import Data.Maybe (fromJust)

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

hashSet :: Show a => S.Set a -> Int32
hashSet = hashString.concat.map show.S.toList

randPartition :: Ord a => [Double] -> Int -> S.Set a -> ([S.Set a],[Double])
randPartition ps n s = (,ps').map S.fromList.M.elems.
                       M.fromListWith (++).zip bins.map (:[]).S.toList $ s
    where (bins,ps') = let (lhs,rhs) = splitAt (S.size s) ps
                       in (map toBin lhs,rhs)
          binSize = 1.0/(fromIntegral n)
          binMap = map (\x -> (x,((binSize-1) * fromIntegral x,
                                binSize * fromIntegral x))) [1..n]
          toBin d = fst.fromJust.find (\(b,(l,r)) -> d>=l && d<r) $ binMap

randCoprimeFactors :: Ord a => [Double] -> S.Set a -> S.Set a ->
                      ([Double],Maybe (S.Set a,S.Set a))
randCoprimeFactors ps iden d
    | diffsz == 1 = (ps,Nothing)
    | otherwise = (ps',Just (p1,p2))
    where diff = S.difference iden d
          diffsz = fromIntegral $ S.size diff
          (r:rands,ps') = splitAt (S.size diff+1) ps
          takeSize = ceiling (r*diffsz)
          bools = map ((<=takeSize).floor.(*diffsz)) rands
          (pa,pb) = partition' fst (zip bools (S.toList diff))
          p1 = S.union d $ S.fromList $ map snd pa
          p2 = S.union d $ S.fromList $ map snd pb
          partition' f xs
              | null p1 = ([head p2],tail p2)
              | null p2 = (tail p1,[head p1])
              | otherwise = (p1,p2)
              where (p1,p2) = partition f xs

tests =
    [
     testGroup "Coprime tests"
       [
        testProperty "nonempty" prop_coprime_nonempty
       ,testProperty "empty" prop_coprime_empty
       ]
    ]

prop_coprime_nonempty =
    forAll (choose (0,40)) $ \divSize ->
    forAll (vectorOf 60 $ choose (0,1)) $ \ps ->
    let iden = S.fromList [0..50::Int]
        d = S.fromList.take divSize $ [0..50]
        (_,Just (x,y)) = randCoprimeFactors ps iden d
    in S.union x y == iden && S.intersection x y == d &&
       x /= d && y /= d

prop_coprime_empty ps =
    forAll (vectorOf 50 $ choose (1,70::Int)) $ \elems  ->
    let iden = S.fromList elems
        d = S.deleteMin iden
        (ps',Nothing) = randCoprimeFactors ps iden d
    in ps == ps'
