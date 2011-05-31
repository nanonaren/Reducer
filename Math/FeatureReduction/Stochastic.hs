module Math.FeatureReduction.Stochastic
    (
      runR
    ) where

import Prelude hiding (log)
--import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random hiding (split,fromList)
import Data.List (partition)
import Data.Maybe (fromJust)
import Math.FeatureReduction.Features
import Math.FeatureReduction.BaseSt (Phi,Value)
import NanoUtils.Set (randPick)
import Avg
import Data.Monoid

data Env m = Env
    {
      psi :: Features -> m Value
    , numSamples :: Int
    , foundIrreducible :: Features -> Int -> Int -> m ()
    , chooseElement :: Features -> Int -> m (Maybe Int)
    , info :: String -> m ()
    , reductionFactor :: Avg Double
    }

type R g m = RandT g (StateT (Env m) m)

runR allFeatures phi numSamples call choose info target fs gen =
    evalStateT runRand env
    where env = Env (toPsi allFeatures phi target)
                    numSamples call choose info (Avg (1,2))
          runRand = evalRandT (complete fs) gen

complete :: (RandomGen g, Monad m) => Features -> R g m Features
complete fs = do
  complete' 300 fs
--  fs' <- level1 fs
--  if size fs' == 0
--   then return fs'
--   else complete' 2 fs'

complete' lvl fs = do
  stop <- stopAlg fs
  case stop of
    True -> return fs
    False -> sample lvl fs >>= \res ->
             case res of
               Nothing -> complete' nextLvl fs
               Just (x,irred) -> do
                        let fs' = diff fs (fromList [x])
                            lvl' = lvl-1
                        complete' lvl' fs'
    where nextLvl = if floor (fromIntegral lvl*1.6) > sz fs
                     then sz fs
                     else floor (fromIntegral lvl*1.6)
          sz = size

stopAlg :: (RandomGen g,Monad m) => Features -> R g m Bool
stopAlg fs = do
  case size fs of
    0 -> return True
    _ -> score fs >>= return.(==0)

level1 :: (RandomGen g,Monad m) => Features -> R g m Features
level1 fs = do
  nonZeros <- evalAndPart.split 1 $ fs
  mapM_ (\f -> reportIrreducible f (head $ toList f) 1) nonZeros
  return.diff fs.unions $ nonZeros

evalAndPart :: (RandomGen g,Monad m) => [Features] -> R g m [Features]
evalAndPart fss = do
  xs <- mapM score fss >>= return.zip fss
  return.map fst.fst.partition ((>0).snd) $ xs

sample :: (RandomGen g,Monad m) => Int -> Features ->
          R g m (Maybe (Int,Features))
sample k fs = do
  num <- lift $ gets (numSamples)
  sets <- wrapRand $ sequence (replicate num (randSubset k fs))
  pickElement k sets

pickElement :: (RandomGen g, Monad m) => Int -> [Features] ->
               R g m (Maybe (Int,Features))
pickElement _ [] = return Nothing
pickElement lvl (f:fs) = do
  s <- score f
  choose <- lift $ gets chooseElement
  case s > 0 of
    True -> getIrreducible f >>= \irred ->
            lift (lift $ choose irred lvl) >>= \c ->
            (if c == Nothing
             then wrapRand (randPick (size irred) (toList irred))
             else return (fromJust c,[])) >>= \(p,_) ->
            reportIrreducible irred p lvl >>
            return (Just (p,irred))
    False -> pickElement lvl fs

getIrreducible :: (RandomGen g, Monad m) => Features -> R g m Features
getIrreducible fs = do
  log $ "in getIrreducible with size " ++ show (size fs)
  case size fs of
    1 -> return fs
    _ -> do
      sub <- search fs
      case sub of
        Nothing -> log "did not find any" >> return fs
        (Just fs') -> getIrreducible fs'

log str = do
  inf <- lift (gets info)
  lift (lift $ inf str)

search fs = do
  let sz = size fs
  factor <- lift (gets reductionFactor)
  log $ "Using factor: " ++ show factor
  search' fs sz (floor $ fromIntegral sz / getAvg factor) 1
search' fs sz _ 5 = log "Number of attempts expired" >> return Nothing
search' fs _ 0 _ = log "Hit 0" >> return Nothing
search' fs sz num attemptNum = do
  log $ "Attempt #" ++ show attemptNum ++ "; trying to remove " ++ show num
  lst <- leaveBunchOuts fs (sz - num)
  sub <- firstM (\f -> score f >>= return.(>0)) lst
  let nextNum = let n = num `div` 2
                in if n == 0 then 1 else n
  case sub of
    Nothing -> search' fs sz nextNum (attemptNum+1)
    Just fs' -> log "Succeeded" >>
                adjustFactor sz num >>
                return (Just fs')

adjustFactor sz num = do
  let fac = fromIntegral sz / fromIntegral num
  red <- lift (gets reductionFactor)
  lift $ modify (\st -> st{reductionFactor = red `mappend` Avg (1,fac)})

leaveBunchOuts :: (RandomGen g, Monad m) => Features -> Int -> R g m [Features]
leaveBunchOuts fs num = do
  wrapRand.sequence $ replicate 10 (randSubset num fs)

reportIrreducible :: (RandomGen g,Monad m) => Features -> Int -> Int -> R g m ()
reportIrreducible fs p lvl = do
  call <- lift $ gets foundIrreducible
  lift.lift $ call fs p lvl

wrapRand act = getSplit >>= return.evalRand act

score fs = do
  f <- lift $ gets psi
  lift.lift.f $ fs

toPsi :: Monad m => Features -> Phi m -> Value -> (Features -> m Value)
toPsi fs phi target =
    \set -> do
      val <- liftM (target-) (phi (diff fs set))
      return $ if val < 0 then 0 else val

--Copied from monad-loops library
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM p [] = return Nothing
firstM p (x:xs) = do
  q <- p x
  if q
   then return (Just x)
   else firstM p xs
