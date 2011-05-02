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
import NanoUtils.List (pairUp)

data FeatureInfo m = FeatureInfo
    {
      allFS :: Features
    , phi :: Features -> m Value
    , psi :: Features -> Features -> m Value
    , foundIrreducible :: Features -> Int -> m ()
    }

type St m = StateT (FeatureInfo m) m
type Value = Double
type Phi m = Features -> m Value
type Psi m = Features -> Features -> m Value

-- |Construct the Psi function
phiToPsi :: Monad m => Phi m -> Psi m
phiToPsi phi =
    \f1 f2 -> do
        inf <- liftM2 (-) (phi f1) $ phi (diff f1 f2)
        return $ if inf < 0 then 0 else inf

-- |Level 1
level1 :: Monad m => St m Features
level1 = do
  allfs <- gets allFS
  nonZeros <- evalAndPart.split 1 $ allfs
  return (unions nonZeros)

-- |Level 2
level2 :: Monad m => Features -> St m Features
level2 ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart.choose2.split 1 $ fs
  let counts = countMap (concat.map toList $ nonZeros)
  return $ stuff nonZeros counts

-- |level n, n > 2
leveln :: Monad m => Int -> Features -> St m Features
leveln n ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart.choose2.split (div n 2) $ fs
  mapM irreds nonZeros >>= picks.concat

complete :: Monad m => St m Features
complete = complete' (fromList []) 0
complete' ireds n = do
  b <- stopAlg ireds
  case b of
    True -> return ireds
    False -> complete'' ireds n
complete'' _ 0 = level1 >>= \rs -> complete' rs 1
complete'' ireds 1 = level2 ireds >>= \rs ->
                     complete' (rs `union` ireds) 2
complete'' ireds n = leveln (2^n) ireds >>= \rs ->
                     complete' (rs `union` ireds) (n+1)

stopAlg :: Monad m => Features -> St m Bool
stopAlg ireds = do
  allfs <- gets allFS
  comp <- complement ireds
  evalPsi allfs comp >>= return.(==0)

evalPsi :: Monad m => Features -> Features -> St m Value
evalPsi context x = do
  f <- gets psi
  lift (f context x)

-- | set complement of X in F
complement :: Monad m => Features -> St m Features
complement ireds = gets allFS >>= return.flip diff ireds

-- |Apply psi to list and partition into zero and non-zero
evalAndPart :: Monad m => [Features] -> St m [Features]
evalAndPart xss = do
  xs <- psiMap xss
  return.map fst.fst.partition ((>0).snd) $ xs

-- |Pick from a list of irreds
picks :: Monad m => [Features] -> St m Features
picks [] = return (fromList [])
picks xss = do
  p <- mapM pick xss >>= return.maximumBy (compare `on` snd)
  liftM (add (fst p)).picks $ filter (not.member (fst p)) xss

-- |Pick an element from an irreducible
-- TODO: change to phi *NOT* psi
pick :: Monad m => Features -> St m (Int,Double)
pick xs = do
  ls <- mapM (evalPsi (fromList [])).split 1 $ xs
  return.maximumBy (compare `on` snd).zip (toList xs).map (*(-1)) $ ls

-- |Get a single irreducible
irred :: Monad m => Features -> St m Features
irred xs = irreds xs >>= return.head

-- |Get all irreducibles
irreds :: Monad m => Features -> St m [Features]
irreds xs = do
  cands <- psiMap (leaveOneOuts xs) >>=
           return.map fst.filter ((>0).snd)
  case cands of
    [] -> return [xs]
    xs -> foldMapM irreds cands

psiMap :: Monad m => [Features] -> St m [(Features,Double)]
psiMap xs = do
  allfs <- gets allFS
  mapM (\f -> liftM (f,).evalPsi allfs $ f) $ xs

stuff :: [Features] -> M.Map Int Int -> Features
stuff [] _ = fromList []
stuff xs counts =
    let ((i,_),counts') = M.deleteFindMax counts
        xs' = filter (not.member i) xs
    in add i (stuff xs' counts')

countMap :: [Int] -> M.Map Int Int
countMap = M.fromListWith (+).flip zip (repeat 1)
