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
                      test_complete_one,test_complete_two,
                      test_complete_three,test_complete_four,
                      test_complete_five,test_complete_six,
                      test1,test2,test3,test4,test5,test6]
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
              [6,7,8,9,10]

test_level2_one =
    runIdentity (evalStateT (level2 (samplePsi 40) (fromList [])) sample) ==
    [10,9]

test_level2_two =
    runIdentity (evalStateT (level2 (samplePsi 30) (fromList [6..10])) sample) == []

test_level3_one =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [7..10])) sample) == [6]

test_level3_two =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [1,8,9,10])) sample) == [7]

test_complete_one =
    runIdentity (evalStateT (complete (samplePsi 46)) sample) == [4..10]

test_complete_two =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo (reverse [1..10]))) == [4..10]

test_complete_three =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo [1,20,10,5,9,2,7])) == [7,9,10,20]

test_complete_four =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo (reverse [1,20,10,5,9,2,7]))) == [7,9,10,20]

test_complete_five =
    runIdentity (evalStateT (complete (samplePsi 55)) (FeatureInfo [1..10])) == [1..10]

test_complete_six =
    runIdentity (evalStateT (complete (samplePsi 30)) (FeatureInfo [1..10])) == [7..10]

test1 =
    runIdentity (evalStateT (level1 (samplePsi 20)) (FeatureInfo [1..10])) == []

test2 =
    runIdentity (evalStateT (level2 (samplePsi 20) (fromList [])) (FeatureInfo [1..10])) == []

test3 =
    runIdentity (evalStateT (leveln (samplePsi 20) 4 (fromList [])) (FeatureInfo [1..10])) == []

test4 =
    runIdentity (evalStateT (leveln (samplePsi 20) 8 (fromList [])) (FeatureInfo [1..10])) == [10,8]

test5 =
    runIdentity (evalStateT (leveln (samplePsi 20) 16 (fromList [8,10])) (FeatureInfo [1..10])) == [9]

test6 =
    runIdentity (evalStateT (complete (samplePsi 20)) (FeatureInfo [1..10])) == [8,9,10]

runOnce = fmap isSuccess.quickCheckWithResult (stdArgs{maxSuccess=1})

sample = FeatureInfo {allFS = [1..10]}
samplePhi :: Double -> Features -> Identity Value
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x
samplePsi n = phiToPsi (samplePhi n)
