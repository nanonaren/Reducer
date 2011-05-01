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

data FeatureInfo = FeatureInfo
    {
      allFS :: Features
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
level1 :: Monad m => Psi m -> St m Features
level1 psi = do
  allfs <- gets allFS
  nonZeros <- evalAndPart psi.split 1 $ allfs
  return (unions nonZeros)

-- |Level 2
level2 :: Monad m => Psi m -> Features -> St m Features
level2 psi ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart psi.choose2.split 1 $ fs
  let counts = countMap (concat.map toList $ nonZeros)
  return $ stuff nonZeros counts

-- |level n, n > 2
leveln :: Monad m => Psi m -> Int -> Features -> St m Features
leveln psi n ireds = do
  fs <- complement ireds
  nonZeros <- evalAndPart psi.choose2.split (div n 2) $ fs
  mapM (irreds psi) nonZeros >>= picks psi.concat

complete :: Monad m => Psi m -> St m Features
complete psi = complete' psi (fromList []) 0
complete' psi ireds n = do
  b <- stopAlg psi ireds
  case b of
    True -> return ireds
    False -> complete'' psi ireds n
complete'' psi _ 0 = level1 psi >>= \rs -> complete' psi rs 1
complete'' psi ireds 1 = level2 psi ireds >>= \rs ->
                         complete' psi (rs `union` ireds) 2
complete'' psi ireds n = leveln psi (2^n) ireds >>= \rs ->
                         complete' psi (rs `union` ireds) (n+1)

stopAlg :: Monad m => Psi m -> Features -> St m Bool
stopAlg psi ireds = do
  allfs <- gets allFS
  comp <- complement ireds
  lift (psi allfs comp) >>= return.(==0)

-- | set complement of X in F
complement :: Monad m => Features -> St m Features
complement ireds = gets allFS >>= return.flip diff ireds

-- |Apply psi to list and partition into zero and non-zero
evalAndPart :: Monad m => Psi m -> [Features] -> St m [Features]
evalAndPart psi xss = do
  xs <- psiMap psi xss
  return.map fst.fst.partition ((>0).snd) $ xs

-- |Pick from a list of irreds
picks :: Monad m => Psi m -> [Features] -> St m Features
picks psi [] = return (fromList [])
picks psi xss = do
  p <- mapM (pick psi) xss >>= return.maximumBy (compare `on` snd)
  liftM (add (fst p)).picks psi $ filter (not.member (fst p)) xss

-- |Pick an element from an irreducible
-- TODO: change to phi *NOT* psi
pick :: Monad m => Psi m -> Features -> St m (Int,Double)
pick psi xs = do
  ls <- lift $ mapM (psi (fromList [])).split 1 $ xs
  return.maximumBy (compare `on` snd).zip (toList xs).map (*(-1)) $ ls

-- |Get a single irreducible
irred :: Monad m => Psi m -> Features -> St m Features
irred psi xs = irreds psi xs >>= return.head

-- |Get all irreducibles
irreds :: Monad m => Psi m -> Features -> St m [Features]
irreds psi xs = do
  cands <- psiMap psi (leaveOneOuts xs) >>=
           return.map fst.filter ((>0).snd)
  case cands of
    [] -> return [xs]
    xs -> foldMapM (irreds psi) cands

psiMap :: Monad m => Psi m -> [Features] -> St m [(Features,Double)]
psiMap psi xs = do
  allfs <- gets allFS
  lift.mapM (\f -> liftM (f,).psi allfs $ f) $ xs

stuff :: [Features] -> M.Map Int Int -> Features
stuff [] _ = fromList []
stuff xs counts =
    let ((i,_),counts') = M.deleteFindMax counts
        xs' = filter (not.member i) xs
    in add i (stuff xs' counts')

countMap :: [Int] -> M.Map Int Int
countMap = M.fromListWith (+).flip zip (repeat 1)

