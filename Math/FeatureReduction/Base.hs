{-# LANGUAGE TupleSections #-}
module Math.FeatureReduction.Base
    (
      FeatureInfo (..)
    , Value
    , phiToPsi
    , level1
    ) where

import Control.Monad.State
import Data.List (partition,intersect)
import Data.Functor.Identity
import Math.FeatureReduction.Features
import NanoUtils.Tuple

data FeatureInfo = FeatureInfo
    {
      allFS :: [Int]
    }

type St = StateT FeatureInfo
type Value = Double
type Phi m = Features -> m Value
type Psi m = Features -> Features -> m Value

-- |Extracts a minimal subset that maximizes phi
--reduce :: Phi m -> Features -> m FeatureInfo

-- |Construct the Psi function
phiToPsi :: Monad m => Phi m -> Psi m
phiToPsi phi =
    \f1 f2 -> do
        inf <- liftM2 (-) (phi f1) $ phi (diff f1 f2)
        return $ if inf < 0 then 0 else inf

-- |Level 1
level1 :: Monad m => Psi m -> St m (Features,Features)
level1 psi = do
  allfs <- gets allFS
  let fs = fromList allfs
  xs <- lift.mapM (\f -> liftM (f,).psi fs.fromList.(:[]) $ f) $ allfs
  return.mapHomTup (fromList.map fst).partition ((>0).snd) $ xs
