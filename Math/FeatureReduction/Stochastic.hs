module Math.FeatureReduction.Stochastic
    (
      runR
    ) where

import Control.Monad.Reader
import Control.Monad.Random hiding (split,fromList)
import Data.List (partition)
import Math.FeatureReduction.Features
import Math.FeatureReduction.BaseSt (Phi,Value)
import NanoUtils.Set (randPick)

data Env m = Env
    {
      psi :: Features -> m Value
    , numSamples :: Int
    , foundIrreducible :: Features -> Int -> Int -> m ()
    }

type R g m = RandT g (ReaderT (Env m) m)

runR allFeatures phi numSamples call target fs gen = runReaderT runRand env
    where env = Env (toPsi allFeatures phi target) numSamples call
          runRand = evalRandT (complete fs) gen

complete :: (RandomGen g, Monad m) => Features -> R g m Features
complete fs = do
--  complete' 1 fs
  fs' <- level1 fs
  if size fs' == 0
   then return fs'
   else complete' 2 fs'

complete' lvl fs = do
  stop <- stopAlg fs
  case stop of
    True -> return fs
    False -> sample lvl fs >>= \res ->
             case res of
               Nothing -> complete' nextLvl fs
               Just (x,lvl') -> let fs' = diff fs (fromList [x])
                                in complete' lvl' fs'
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

sample :: (RandomGen g,Monad m) => Int -> Features -> R g m (Maybe (Int,Int))
sample k fs = do
  num <- lift $ asks (numSamples)
  sets <- wrapRand $ sequence (replicate num (randSubset k fs))
  pickElement k sets

pickElement :: (RandomGen g, Monad m) => Int -> [Features] -> R g m (Maybe (Int,Int))
pickElement _ [] = return Nothing
pickElement lvl (f:fs) = do
  s <- score f
  case s > 0 of
    True -> getIrreducible f >>= \irred ->
            wrapRand (randPick (size irred) (toList irred)) >>= \(p,_) ->
            reportIrreducible irred p lvl >>
            return (Just (p,size irred))
    False -> pickElement lvl fs

getIrreducible :: (RandomGen g, Monad m) => Features -> R g m Features
getIrreducible fs = do
  case size fs of
    1 -> return fs
    _ -> do
      sub <- firstM (\f -> score f >>= return.(>0)) (leaveOneOuts fs)
      case sub of
        Nothing -> return fs
        (Just fs') -> getIrreducible fs'

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
