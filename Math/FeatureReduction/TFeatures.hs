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
    mapM (fmap isSuccess.quickCheckResult) [prop_id,prop_diff]

genInts = listOf (choose (1,100))

-- |id = toList.fromList
prop_id =
    forAll genInts $ \xs -> (==nub (sort xs)).toList.fromList $ xs

-- |check diff function
prop_diff =
    forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
    toList (fromList xs `diff` fromList ys) == sort (nub xs \\ nub ys)
