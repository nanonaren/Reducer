module Math.FeatureReduction.TFeatures
    (
      testFeatures
    ) where

import Math.FeatureReduction.Features
import Data.List ((\\),sort,nub)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

testFeatures = [testGroup "Features"
               [
                 testProperty "convert to and from" prop_id
               , testProperty "diff" prop_diff
               , testProperty "size" prop_size
               ]]

genInts = fmap (sort.nub) $ listOf (choose (1,100))

-- |id = toList.fromList
prop_id =
    forAll genInts $ \xs -> (==xs).toList.fromList $ xs

-- |check diff function
prop_diff =
    forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
    toList (fromList xs `diff` fromList ys) == xs \\ ys

-- |check size reports
prop_size =
    forAll genInts $ \xs -> (==length xs).size.fromList $ xs