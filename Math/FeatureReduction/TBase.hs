module Math.FeatureReduction.TBase
    (
      testBase
    ) where

import Math.FeatureReduction.Features
import Math.FeatureReduction.Base
import Data.Functor.Identity
import Control.Monad.State
import qualified Data.BitSet as B
import Data.List (sort,nub,(\\),intersect)
import Test.QuickCheck hiding (sample)
import Test.QuickCheck.Test

testBase = do
  xs <- mapM (fmap isSuccess.quickCheckResult)
        [prop_phiToPsi,prop_phiToPsi_zeroBound]
  o <- runOnce test_level1
  return.and $ o:xs

genInts = fmap (sort.nub) $ listOf (choose (1,100))

-- |Test phi to psi conversion
prop_phiToPsi = do
  let psi = phiToPsi (return.fromIntegral.B.size)
  forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
      runIdentity (psi (fromList xs) (fromList ys)) ==
      fromIntegral (length xs - length (xs \\ ys))

-- |Test that psi doesn't go below 0
prop_phiToPsi_zeroBound = do
  let psi = phiToPsi (return.(1/).(+1).fromIntegral.B.size)
  forAll (genInts >>= \xs -> genInts >>= \ys -> return (xs,ys)) $ \(xs,ys) ->
      runIdentity (psi (fromList xs) (fromList ys)) >= 0

test_level1 = runIdentity (evalStateT (level1 samplePsi) sample) ==
              (fromList [2,5,7,9],fromList [0,1,3,4,6,8,10])

runOnce = fmap isSuccess.quickCheckWithResult (stdArgs{maxSuccess=1})

sample = FeatureInfo {allFS = [0..10]}
samplePhi :: Features -> Identity Value
samplePhi = return.fromIntegral.length.intersect [2,5,7,9].toList
samplePsi = phiToPsi samplePhi
