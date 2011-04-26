{-# LANGUAGE TupleSections #-}
module Math.FeatureReduction.Base
    (
      FeatureInfo (..)
    , Value
    , phiToPsi
    , level1
    , level2
    ) where

import Control.Monad.State
import Data.List (partition,intersect)
import qualified Data.Map as M
import Data.Functor.Identity
import Math.FeatureReduction.Features
import NanoUtils.Tuple
import NanoUtils.List (choose2)

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
  xs <- lift.mapM (\f -> liftM (f,).psi fs.fromList.(:[]) $ f) $ allfs --R
  return.mapHomTup (fromList.map fst).partition ((>0).snd) $ xs

-- |Level 2
level2 :: Monad m => Psi m -> Features -> St m [Int]
level2 psi fs = do
  let fsLst = toList fs
  xs <- lift.mapM (\f -> liftM (f,).psi fs.fromList $ f) $ choose2 fsLst --R
  let (nonZeros,zeros) = mapHomTup (map fst).partition ((>0).snd) $ xs
      counts = countMap (concat nonZeros)
  return $ stuff nonZeros counts

stuff :: Ord a => [[a]] -> M.Map a Int -> [a]
stuff [] _ = []
stuff xs counts =
    let ((i,_),counts') = M.deleteFindMax counts
        xs' = filter (not.elem i) xs
    in i : stuff xs' counts'

countMap :: Ord a => [a] -> M.Map a Int
countMap = M.fromListWith (+).flip zip (repeat 1)