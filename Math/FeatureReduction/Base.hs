{-# LANGUAGE TupleSections #-}
module Math.FeatureReduction.Base
    (
      FeatureInfo (..)
    , Value
    , phiToPsi
    , level1
    , level2
    , leveln
    , complete
    ) where

import Control.Monad.State
import Data.List (partition,intersect,splitAt,maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import Data.Functor.Identity
import Math.FeatureReduction.Features
import NanoUtils.Container (foldMapM)
import NanoUtils.Tuple
import NanoUtils.List (choose2,pairUp,leaveOneOuts,chunk)

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
  allfs <- gets (fromList.allFS)
  let fsLst = toList fs
  xs <- psiMap psi $ choose2 fsLst --R
  let (nonZeros,zeros) = mapHomTup (map fst).partition ((>0).snd) $ xs
      counts = countMap (concat nonZeros)
  return $ stuff nonZeros counts

-- |level n, n > 2
leveln :: Monad m => Psi m -> Int -> Features -> St m [Int]
leveln psi n fs = do
  allfs <- gets (fromList.allFS)
  xs <- psiMap psi.map concat.choose2.chunk (div n 2).toList $ fs
  let (nonZeros,zeros) = mapHomTup (map fst).partition ((>0).snd) $ xs
  mapM (irred psi) nonZeros >>= picks psi

complete :: Monad m => Psi m -> St m [Int]
complete psi = do
  (r1,ys) <- level1 psi
  r2 <- level2 psi ys
  return (toList r1 ++ r2)

-- |Pick from a list of irreds
picks :: Monad m => Psi m -> [[Int]] -> St m [Int]
picks psi [] = return []
picks psi (xs:xss) = do
  p <- pick psi xs
  liftM (p:).picks psi $ filter (not.elem p) xss

-- |Pick an element from an irreducible
-- TODO: change to phi *NOT* psi
pick :: Monad m => Psi m -> [Int] -> St m Int
pick psi xs = do
  ls <- lift $ mapM (psi (fromList []).fromList.(:[])) xs
  return.fst.maximumBy (compare `on` snd).zip xs.map (*(-1)) $ ls

-- |Get a single irreducible
irred :: Monad m => Psi m -> [Int] -> St m [Int]
irred psi xs = irreds psi xs >>= return.head

-- |Get all irreducibles
irreds :: Monad m => Psi m -> [Int] -> St m [[Int]]
irreds psi xs = do
  cands <- psiMap psi (leaveOneOuts xs) >>=
           return.map fst.filter ((>0).snd)
  case cands of
    [] -> return [xs]
    xs -> foldMapM (irreds psi) cands

psiMap :: Monad m => Psi m -> [[Int]] -> St m [([Int],Double)]
psiMap psi xs = do
  allfs <- gets (fromList.allFS)
  lift.mapM (\f -> liftM (f,).psi allfs.fromList $ f) $ xs

stuff :: Ord a => [[a]] -> M.Map a Int -> [a]
stuff [] _ = []
stuff xs counts =
    let ((i,_),counts') = M.deleteFindMax counts
        xs' = filter (not.elem i) xs
    in i : stuff xs' counts'

countMap :: Ord a => [a] -> M.Map a Int
countMap = M.fromListWith (+).flip zip (repeat 1)
