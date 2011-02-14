{-# LANGUAGE TupleSections #-}
module SetUtils
    (
      randCoprimeFactors
    , randPartition
    ) where

import Control.Monad.Random
import Data.Int
import Data.List (partition)
import ListUtils (sortOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.HashTable (hashString)

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

hashSet :: Show a => S.Set a -> Int32
hashSet = hashString.concat.map show.S.toList
{-
randPermutation :: Int -> [a] -> Rand g [a]
randPermutation n xs = do
  xs' <- mapM (\x -> getRandomR (1,n) >>= \r -> return (x,r)) xs
  return $ sortOn snd bxs'
-}
randPartition :: (RandomGen g,Ord a) => Int -> S.Set a -> Rand g [S.Set a]
randPartition n s = do
  binMap <- mapM (\x -> getRandomR (1,n) >>= \r -> return (r,[x])).S.toList $ s
  return.map S.fromList.M.elems.M.fromListWith (++) $ binMap

randCoprimeFactors :: (RandomGen g,Ord a) =>
                      S.Set a
                   -> S.Set a
                   -> Rand g (Maybe (S.Set a,S.Set a))
randCoprimeFactors iden d = do
  let diff = S.difference iden d
  case S.size diff of
    1 -> return Nothing
    _ -> do
      parts <- randPartition 2 diff
      return (Just $ addDiv $ balance parts)
    where balance ps
              | length ps == 1 = moveOne S.empty (head ps)
              | otherwise = (head ps,(head.tail) ps)
          moveOne s1 s2 = let (a,s2') = S.deleteFindMin s2
                          in (S.insert a s1,s2')
          addDiv (a,b) = (S.union d a,S.union d b)

{-
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
-}