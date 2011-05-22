module Math.FeatureReduction.TBase
    (
      testBase
    ) where

import Math.FeatureReduction.Features
import Math.FeatureReduction.Base
import Data.Functor.Identity
import qualified Data.Map as M
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
      , testCase "complete3" sumC3
      , testCase "complete4" sumC4
      , testCase "complete5" sumC5
      , testCase "complete6" sumC6
      , testCase "sumL1_2" sumL1_2
      , testCase "sumL2_3" sumL2_3
      , testCase "sumL3_3" sumL3_3
      , testCase "sumL4" sumL4
      , testCase "sumL5" sumL5
      , testCase "sumC7" sumC7
      ]
    , testGroup "Penalty Sum"
      [
        testCase "psumL1" testa
      , testCase "psumL2" testb
      , testCase "psumL3" testc
      , testCase "psumL4" testd
      , testCase "psumL5" teste
      , testCase "complete" testf
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

sumL1 = runIdentity (evalStateT level1 (sample 50)) @?=
              fromList [6,7,8,9,10]

sumL2_1 =
    runIdentity (evalStateT (level2 (fromList [])) (sample 40)) @?=
                fromList [10,9]

sumL2_2 =
    runIdentity (evalStateT (level2 (fromList [6..10])) (sample 30)) @?=
                fromList []

sumL3_1 =
    runIdentity (evalStateT (leveln 4 (fromList [7..10])) (sample 40)) @?=
                fromList [6]

sumL3_2 =
    runIdentity (evalStateT (leveln 4 (fromList [1,8,9,10])) (sample 40)) @?=
                fromList [7]

sumC1 =
    runIdentity (evalStateT complete (sample 46)) @?=
                fromList [4..10]

sample3 n = dummySample {allFS = fromList [1,20,10,5,9,2,7],phi=samplePhi n,psi=samplePsi n}
dummySample = FeatureInfo {allFS=undefined,phi=undefined,psi=undefined,
                           foundIrreducible=dummy,lvl1and2=fromList [],
                           workingSet=fromList [],pickedAtLvl=M.empty,
                           info=(\_ -> return ())}
sumC3 =
    runIdentity (evalStateT complete (sample3 46)) @?=
                fromList [7,9,10,20]
sumC4 =
    runIdentity (evalStateT complete (sample3 46)) @?=
                fromList [7,9,10,20]

sumC5 =
    runIdentity (evalStateT complete (sample 55)) @?=
                fromList [1..10]

sumC6 =
    runIdentity (evalStateT complete (sample 30)) @?=
                fromList [7..10]

sumL1_2 =
    runIdentity (evalStateT level1 (sample 20)) @?=
                fromList []

sumL2_3 =
    runIdentity (evalStateT (level2 (fromList [])) (sample 20)) @?=
                fromList []

sumL3_3 =
    runIdentity (evalStateT (leveln 4 (fromList [])) (sample 20)) @?=
                fromList []

sumL4 =
    runIdentity (evalStateT (leveln 8 (fromList [])) (sample 20)) @?=
                fromList [10,8]

sumL5 =
    runIdentity (evalStateT (leveln 16 (fromList [8,10])) (sample 20)) @?=
                fromList [9]

sumC7 =
    runIdentity (evalStateT complete (sample 20)) @?=
                fromList [8,9,10]

sample n = dummySample {allFS = fromList [1..10],phi=samplePhi n,psi=samplePsi n}
samplePhi :: Double -> Features -> Identity Value
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x
samplePsi n = phiToPsi (samplePhi n)

testa =
    runIdentity (evalStateT level1 (sample2 46)) @?=
                fromList []

testb =
    runIdentity (evalStateT (level2 (fromList [])) (sample2 46)) @?=
                fromList [10]

setWS xs = modify (\st -> st{workingSet=fromList xs})

--check =
--    runIdentity (evalStateT (setWS [] >> level1) (sample2 46))

testc =
    runIdentity (evalStateT (setWS [10] >> leveln 4 (fromList [10])) (sample2 46)) @?=
                fromList [9,8]

testd =
    runIdentity (evalStateT (setWS [10,9,8] >> leveln 8 (fromList [8,9,10])) (sample2 46)) @?=
                fromList [7]

teste =
    runIdentity (evalStateT (setWS [10,9,8,7] >> leveln 16 (fromList [7,8,9,10])) (sample2 46)) @?=
                fromList [6]

testf =
    runIdentity (evalStateT complete (sample2 46)) @?=
                fromList [6..10]

sample2 n = dummySample {allFS = fromList [1..10],phi=samplePhi2 n,psi=samplePsi2 n}
samplePhi2 :: Int -> Features -> Identity Value
samplePhi2 n = return.fromIntegral.penalize.sum.toList
    where pick x | x > n = n
                 | otherwise = x
          penalize v = if v-n > 0
                       then if v-n > n then 0 else n - (v-n)
                       else v
samplePsi2 n = phiToPsi (samplePhi2 n)

dummy _ _ = return ()