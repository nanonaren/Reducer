module Math.FeatureReduction.TBase
    (
      testBase
    ) where

import Math.FeatureReduction.Features
import Math.FeatureReduction.Base
import Data.Functor.Identity
import Control.Monad.State
import Data.List (sort,nub,(\\),intersect)
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Test

testBase = do
  xs <- mapM (fmap isSuccess.quickCheckResult)
        [prop_phiToPsi,prop_phiToPsi_zeroBound]
--  o1 <- runOnce test_level1
  o2 <- runOnce test_level2
  return.and $ o2:xs

genInts = fmap (sort.nub) $ listOf (choose (1,100))

-- |Test phi to psi conversion
prop_phiToPsi = do
  let psi = phiToPsi (return.fromIntegral.size)
  forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
      runIdentity (psi (fromList xs) (fromList ys)) ==
      fromIntegral (length xs - length (xs \\ ys))

-- |Test that psi doesn't go below 0
prop_phiToPsi_zeroBound = do
  let psi = phiToPsi (return.(1/).(+1).fromIntegral.size)
  forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
      runIdentity (psi (fromList xs) (fromList ys)) >= 0

test_level1 = runIdentity (evalStateT (level1 (samplePsi 50)) sample) ==
              (fromList [6,7,8,9,10],fromList [0..5])

test_level2 =
    runIdentity (evalStateT (level2 (samplePsi 40) (fromList [0..10])) sample) ==
    [10,9]

runOnce = fmap isSuccess.quickCheckWithResult (stdArgs{maxSuccess=1})

sample = FeatureInfo {allFS = [0..10]}
samplePhi :: Double -> Features -> Identity Value
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x
samplePsi n = phiToPsi (samplePhi n)
