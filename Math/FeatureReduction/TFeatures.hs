module Math.FeatureReduction.TFeatures
    (
      testFeatures
    ) where

import Math.FeatureReduction.Features
import Data.List ((\\),sort,nub)
import Test.QuickCheck
import Test.QuickCheck.Test

testFeatures =
    fmap and $
    mapM (fmap isSuccess.quickCheckResult) [prop_id,prop_diff,prop_size]

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