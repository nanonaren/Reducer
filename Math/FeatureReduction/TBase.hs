module Math.FeatureReduction.TBase
    (
      testBase
    ) where

import Math.FeatureReduction.Features
import Math.FeatureReduction.Base
import Data.Functor.Identity
import Control.Monad.State
import Data.List (sort,nub,(\\),intersect)
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (sample)
import Test.HUnit

testBase =
    [
      testGroup "Phi and Psi"
      [
        testProperty "phi to psi" prop_phiToPsi
      , testProperty "psi zero" prop_phiToPsi_zeroBound
      ]
    , testGroup "Sum"
      [
        testCase "level1" sumL1
      , testCase "level2_1" sumL2_1
      , testCase "level2_2" sumL2_2
      , testCase "level3_1" sumL3_1
      , testCase "level3_2" sumL3_2
      , testCase "complete1" sumC1
      , testCase "complete2" sumC2
      , testCase "complete3" sumC3
      , testCase "complete4" sumC4
      , testCase "complete5" sumC5
      , testCase "complete6" sumC6
      ]
    , testGroup "Penalty Sum"
      [
        testCase "level1" psumL1
      , testCase "level2" psumL2
      , testCase "level3" psumL3
      , testCase "level4" psumL4
      , testCase "level5" psumL5
      , testCase "complete" psumC1
      ]
    ]

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

sumL1 = runIdentity (evalStateT (level1 (samplePsi 50)) sample) @?=
              [6,7,8,9,10]

sumL2_1 =
    runIdentity (evalStateT (level2 (samplePsi 40) (fromList [])) sample) @?=
    [10,9]

sumL2_2 =
    runIdentity (evalStateT (level2 (samplePsi 30) (fromList [6..10])) sample) @?= []

sumL3_1 =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [7..10])) sample) @?= [6]

sumL3_2 =
    runIdentity (evalStateT (leveln (samplePsi 40) 4 (fromList [1,8,9,10])) sample) @?= [7]

sumC1 =
    runIdentity (evalStateT (complete (samplePsi 46)) sample) @?= [4..10]

sumC2 =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo (reverse [1..10]))) @?= [4..10]

sumC3 =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo [1,20,10,5,9,2,7])) @?= [7,9,10,20]

sumC4 =
    runIdentity (evalStateT (complete (samplePsi 46)) (FeatureInfo (reverse [1,20,10,5,9,2,7]))) @?= [7,9,10,20]

sumC5 =
    runIdentity (evalStateT (complete (samplePsi 55)) (FeatureInfo [1..10])) @?= [1..10]

sumC6 =
    runIdentity (evalStateT (complete (samplePsi 30)) (FeatureInfo [1..10])) @?= [7..10]

psumL1 =
    runIdentity (evalStateT (level1 (samplePsi 20)) (FeatureInfo [1..10])) @?= []

psumL2 =
    runIdentity (evalStateT (level2 (samplePsi 20) (fromList [])) (FeatureInfo [1..10])) @?= []

psumL3 =
    runIdentity (evalStateT (leveln (samplePsi 20) 4 (fromList [])) (FeatureInfo [1..10])) @?= []

psumL4 =
    runIdentity (evalStateT (leveln (samplePsi 20) 8 (fromList [])) (FeatureInfo [1..10])) @?= [10,8]

psumL5 =
    runIdentity (evalStateT (leveln (samplePsi 20) 16 (fromList [8,10])) (FeatureInfo [1..10])) @?= [9]

psumC1 =
    runIdentity (evalStateT (complete (samplePsi 20)) (FeatureInfo [1..10])) @?= [8,9,10]

sample = FeatureInfo {allFS = [1..10]}
samplePhi :: Double -> Features -> Identity Value
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x
samplePsi n = phiToPsi (samplePhi n)

testa =
    runIdentity (evalStateT (level1 (samplePsi2 46)) (FeatureInfo [1..10])) == []

testb =
    runIdentity (evalStateT (level2 (samplePsi2 46) (fromList [])) (FeatureInfo [1..10])) == [10]

testc =
    runIdentity (evalStateT (leveln (samplePsi2 46) 4 (fromList [10])) (FeatureInfo [1..10])) == [9,8]

testd =
    runIdentity (evalStateT (leveln (samplePsi2 46) 8 (fromList [8,9,10])) (FeatureInfo [1..10])) == [6,7]

teste =
    runIdentity (evalStateT (leveln (samplePsi2 46) 16 (fromList [6,7,8,9,10])) (FeatureInfo [1..10])) == []

testf =
    runIdentity (evalStateT (complete (samplePsi2 46)) (FeatureInfo [1..10])) == [6..10]

samplePhi2 :: Int -> Features -> Identity Value
samplePhi2 n = return.fromIntegral.penalize.sum.toList
    where pick x | x > n = n
                 | otherwise = x
          penalize v = if v-n > 0
                       then if v-n > n then 0 else n - (v-n)
                       else v
samplePsi2 n = phiToPsi (samplePhi2 n)
