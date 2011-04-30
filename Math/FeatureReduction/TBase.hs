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
  os <- mapM runOnce [test_level1,test_level2_one,test_level2_two,
                      test_level3_one,test_level3_two,
                      test_complete_one,test_complete_two]
  return.and $ os ++ xs

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
              (fromList [6,7,8,9,10],fromList [1..5])

test_level2_one =
    runIdentity (evalStateT (level2 (samplePsi 40) (fromList [1..10])) sample) ==
    [10,9]

test_level2_two =
    runIdentity (evalStateT (level2 (samplePsi 30) (fromList [1..5])) sample) == []

test_level3_one =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [1..6])) sample) == [6]

test_level3_two =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [2..7])) sample) == [7]

test_complete_one =
    runIdentity (evalStateT (complete (samplePsi 40)) sample) == [10,9]

test_complete_two =
    runIdentity (evalStateT (complete (samplePsi 46)) sample) == [10,9,8,7,6]

runOnce = fmap isSuccess.quickCheckWithResult (stdArgs{maxSuccess=1})

sample = FeatureInfo {allFS = [1..10]}
samplePhi :: Double -> Features -> Identity Value
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x
samplePsi n = phiToPsi (samplePhi n)
