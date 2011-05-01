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
    , irreds
    ) where

import Control.Monad.State
import Data.List (partition,intersect,splitAt,maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import Data.Functor.Identity
import Math.FeatureReduction.Features
import NanoUtils.Container (foldMapM)
import NanoUtils.Tuple
import NanoUtils.List (pairUp,leaveOneOuts,chunk)

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
level1 :: Monad m => Psi m -> St m [Int]
level1 psi = do
  allfs <- gets allFS
  nonZeros <- evalAndPart psi $ map (:[]) allfs
  return (concat nonZeros)

-- |Level 2
level2 :: Monad m => Psi m -> Features -> St m [Int]
level2 psi ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart psi $ choose2 (toList fs)
  let counts = countMap (concat nonZeros)
  return $ stuff nonZeros counts

-- |level n, n > 2
leveln :: Monad m => Psi m -> Int -> Features -> St m [Int]
leveln psi n ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart psi.map concat.choose2.chunk (div n 2).toList $ fs
  mapM (irreds psi) nonZeros >>= picks psi.concat

complete :: Monad m => Psi m -> St m [Int]
complete psi = complete' psi (fromList []) 0 >>= return.toList
complete' psi ireds n = do
  b <- stopAlg psi ireds
  case b of
    True -> return ireds
    False -> complete'' psi ireds n
complete'' psi _ 0 = level1 psi >>= \rs -> complete' psi (fromList rs) 1
complete'' psi ireds 1 = level2 psi ireds >>= \rs ->
                         complete' psi (fromList $ rs++toList ireds) 2
complete'' psi ireds n = leveln psi (2^n) ireds >>= \rs ->
                        complete' psi (fromList $ rs++toList ireds) (n+1)

stopAlg :: Monad m => Psi m -> Features -> St m Bool
stopAlg psi ireds = do
  allfs <- gets (fromList.allFS)
  comp <- complement ireds
  lift (psi allfs comp) >>= return.(==0)

-- | set complement of X in F
complement :: Monad m => Features -> St m Features
complement ireds = do
  allfs <- gets (fromList.allFS)
  return $ diff allfs ireds

-- |Apply psi to list and partition into zero and non-zero
evalAndPart :: Monad m => Psi m -> [[Int]] -> St m [[Int]]
evalAndPart psi xss = do
  xs <- psiMap psi xss
  return.map fst.fst.partition ((>0).snd) $ xs

-- |Pick from a list of irreds
picks :: Monad m => Psi m -> [[Int]] -> St m [Int]
picks psi [] = return []
picks psi xss = do
  p <- mapM (pick psi) xss >>= return.maximumBy (compare `on` snd)
  liftM (fst p:).picks psi $ filter (not.elem (fst p)) xss

-- |Pick an element from an irreducible
-- TODO: change to phi *NOT* psi
pick :: Monad m => Psi m -> [Int] -> St m (Int,Double)
pick psi xs = do
  ls <- lift $ mapM (psi (fromList []).fromList.(:[])) xs
  return.maximumBy (compare `on` snd).zip xs.map (*(-1)) $ ls

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

-- |Special choose 2, because if only one elem it will return it
-- I need it to work like this
choose2 [] = []
choose2 (x:[]) = [[x]]
choose2 (x:xs) = [[x,y]|y<-xs] ++ choose2 xs