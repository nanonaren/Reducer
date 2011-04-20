{-# LANGUAGE FunctionalDependencies,TupleSections,MultiParamTypeClasses #-}
module ChineseRem
    (
      Rem (..)
    , divisorSearch
    , divisorSearch2
    ) where

import Control.Monad

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (sample)

class Monad m => Rem a b m | m -> a b where
    coprimeFactors :: a -> m (Maybe (a,a))
    sample :: a -> m b
    isomorph :: m (a,b) -> m (a,b) -> m b
    update :: a -> b -> m b


--THE MAIN ALGORITHM--
divisorSearch :: (Rem a b m) => a -> m b
divisorSearch a = do
  facs <- coprimeFactors a
  case facs of
    Nothing -> sample a
    Just (x,y) -> liftM (x,) (divisorSearch x) `isomorph`
                  liftM (y,) (divisorSearch y)

divisorSearch2 :: (Rem a b m) => a -> m b
divisorSearch2 a = do
  facs <- coprimeFactors a
  case facs of
    Nothing -> sample a
    Just (x,y) -> do
              b <- liftM (x,) (divisorSearch2 x) `isomorph`
                   liftM (y,) (divisorSearch2 y)
              update a b

-- eta, depth -> eta
eta :: Double -> Int -> Double
eta p 0 = p
eta p d = 1 - (1-e)^2
    where e = eta p (d-1)

-- eta, required eta -> depth
requiredDepth :: Double -> Double -> Int
requiredDepth e req = head.dropWhile ((<req).eta e) $ [0..]



-- TESTS --

tests =
    [
     testGroup "Error Tests"
       [
        testProperty "depth" prop_eta_depth_monotonic
       ,testProperty "prob" prop_eta_prob_monotonic
       ,testProperty "requiredDepth" prop_requiredDepth_monotonic
       ]
    ]

prop_eta_depth_monotonic =
    forAll (choose (0,1)) $ \p ->
    forAll (choose (0,10)) $ \d ->
    forAll (choose (0,10)) $ \d' ->
    let lhs = eta p d
        rhs = eta p d'
    in 
      d <= d' && lhs <= rhs || d > d' && lhs >= rhs

prop_eta_prob_monotonic =
    forAll (choose (0,1)) $ \p ->
    forAll (choose (0,1)) $ \p' ->
    forAll (choose (0,10)) $ \d ->
    let lhs = eta p d
        rhs = eta p' d
    in 
      p <= p' && lhs <= rhs || p > p' && lhs >= rhs

prop_requiredDepth_monotonic =
    forAll (choose (0,1)) $ \e ->
    forAll (choose (0,1)) $ \req ->
    forAll (choose (0,1)) $ \req' ->
    let lhs = requiredDepth e req
        rhs = requiredDepth e req'
    in
      req <= req' && lhs <= rhs || req > req' && lhs >= rhs