module SetUtils
    (
    ) where

import Data.List (partition)
import qualified Data.Set as S

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

randCoprimeFactors :: Ord a => [Bool] -> S.Set a -> S.Set a ->
                      ([Bool],Maybe (S.Set a,S.Set a))
randCoprimeFactors ps iden d
    | diffsz == 1 = (ps,Nothing)
    | otherwise = (ps',Just (p1,p2))
    where diff = S.difference iden d
          diffsz = S.size diff
          (rands,ps') = splitAt diffsz ps
          (pa,pb) = partition' fst (zip rands (S.toList diff))
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
        testProperty "empty" prop_coprime_empty
       ,testProperty "nonempty" prop_coprime_nonempty
       ]
    ]

prop_coprime_nonempty =
    forAll (choose (1,49)) $ \divSize ->
    forAll (vectorOf (51-divSize) $ choose (False,True)) $ \ps ->
    let iden = S.fromList [0..50::Int]
        d = S.fromList.take divSize $ [0..50]
        (ps',Just (x,y)) = randCoprimeFactors ps iden d
    in null ps' && S.union x y == iden && S.intersection x y == d &&
       x /= d && y /= d

prop_coprime_empty ps =
    forAll (vectorOf 50 $ choose (1,70::Int)) $ \elems  ->
    let iden = S.fromList elems
        d = S.deleteMin iden
        (ps',Nothing) = randCoprimeFactors ps iden d
    in ps == ps'