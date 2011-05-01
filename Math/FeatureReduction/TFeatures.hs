module Math.FeatureReduction.TFeatures
    (
      testFeatures
    ) where

import Math.FeatureReduction.Features
import Data.List ((\\),sort,nub)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

testFeatures = [testGroup "Features"
               [
                 testProperty "convert to and from" prop_id
               , testProperty "diff" prop_diff
               , testProperty "union" prop_union
               , testProperty "size" prop_size
               , testCase "chunk" test_chunk
--               , testCase "choose2" test_choose2
               ]]

genInts = fmap (sort.nub) $ listOf (choose (1,100))

-- |id = toList.fromList
prop_id =
    forAll genInts $ \xs -> (==xs).toList.fromList $ xs

-- |check diff function
prop_diff =
    forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
    toList (fromList xs `diff` fromList ys) == xs \\ ys

prop_union =
    forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
    toList (fromList xs `union` fromList ys) == nub (sort $ xs ++ ys)

-- |check size reports
prop_size =
    forAll genInts $ \xs -> (==length xs).size.fromList $ xs

test_chunk =
    map toList (chunk 2 (map fromList.map (:[]) $ [1..11])) @?=
            [[1,2],[3,4],[5,6],[7,8],[9,10],[11]]

test_choose2 =
    map toList (choose2 (map fromList.map (:[]) $ [1..4]))-- @?=
--            [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]