module Math.FeatureReduction.Stochastic
    (
      runR
    ) where

import Prelude hiding (log)
import Control.Monad.Reader
import Control.Monad.Random hiding (split,fromList)
import Data.List (partition)
import Data.Maybe (fromJust)
import Math.FeatureReduction.Features
import Math.FeatureReduction.BaseSt (Phi,Value)
import NanoUtils.Set (randPick)

data Env m = Env
    {
      psi :: Features -> m Value
    , numSamples :: Int
    , foundIrreducible :: Features -> Int -> Int -> m ()
    , chooseElement :: Features -> Int -> m (Maybe Int)
    , clearRemaining :: m Bool
    , info :: String -> m ()
    , leftOut :: Features -> m ()
    }

type R g m = RandT g (ReaderT (Env m) m)

runR allFeatures phi numSamples call choose clear info out target fs gen = runReaderT runRand env
    where env = Env (toPsi allFeatures phi target) numSamples call choose clear info out
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
  clear <- lift (asks clearRemaining)
  out <- lift (asks leftOut)
  case stop of
    True -> return fs
    False -> sample lvl fs >>= \res ->
             case res of
               Nothing -> complete' nextLvl fs
               Just (x,irred) -> do
                        b <- lift (lift clear)
                        let fs' = if b then diff fs irred
                                  else diff fs (fromList [x])
                            lvl' = if b then lvl - size irred else lvl-1
                        when b (lift.lift $ out (diff fs (fromList [x])))
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
  num <- lift $ asks (numSamples)
  sets <- wrapRand $ sequence (replicate num (randSubset k fs))
  pickElement k sets

pickElement :: (RandomGen g, Monad m) => Int -> [Features] ->
               R g m (Maybe (Int,Features))
pickElement _ [] = return Nothing
pickElement lvl (f:fs) = do
  s <- score f
  choose <- lift $ asks chooseElement
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
      lst <- leaveBunchOuts fs
      sub <- firstM (\f -> score f >>= return.(>0)) lst
      case sub of
        Nothing -> log "did not find any" >> return fs
        (Just fs') -> getIrreducible fs'

log str = do
  inf <- lift (asks info)
  lift (lift $ inf str)

leaveBunchOuts :: (RandomGen g, Monad m) => Features -> R g m [Features]
leaveBunchOuts fs = do
  let sz = size fs `div` 2
  wrapRand.sequence $ replicate 3 (randSubset sz fs)

reportIrreducible :: (RandomGen g,Monad m) => Features -> Int -> Int -> R g m ()
reportIrreducible fs p lvl = do
  call <- lift $ asks foundIrreducible
  lift.lift $ call fs p lvl

wrapRand act = getSplit >>= return.evalRand act

score fs = do
  f <- lift $ asks psi
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
