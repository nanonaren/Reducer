module Math.FeatureReduction.Stochastic
    (
      runR
    ) where

import Avg
import Prelude hiding (log)
import Control.Monad.State
import Control.Monad.Random hiding (split,fromList)
import Data.List (partition,maximumBy,intercalate)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Ord
import Math.FeatureReduction.Features
import Math.FeatureReduction.BaseSt (Phi,Value)
import NanoUtils.Set (randPick)

data Env m = Env
    {
      phi :: Features -> m Value
    , psi :: Features -> m Value
    , allFeatures :: Features
    , numSamples :: Int
    , foundIrreducible :: Features -> [Int] -> Int -> m ()
    , chooseElement :: Features -> Int -> m ([Int],[Int])
    , info :: String -> m ()
    , reductionFactor :: Avg Double
    , rejected :: Features
    }

type R g m = RandT g (StateT (Env m) m)

runR allFS phi numSamples call choose info target fs gen =
    evalStateT runRand env
    where env = Env phi (toPsi allFS phi target)
                    allFS numSamples call choose info (Avg (1,2)) (fromList [])
          runRand = evalRandT (complete fs) gen

--search min curr max fs history = do
--  docurrent
  --Each time log the following:
  --1. attempting value
  --2. attempted value
  --3. reduction ratio [size fs : size found]
  --Store the following in history
  --1. is a map with key = iteration number
  --2. value = (attempted value, found value,num features used)
--  case val >= curr of
--    True -> search min' curr' max fs history'
--    False -> search min curr' max' fs history'

complete :: (RandomGen g, Monad m) => Features -> R g m Features
complete fs = do
  let sz = size fs
  when (sz <= 10) (error "Number of features less than 11")
  -- Just being incredibly lazy and annoying
  complete' (size fs - 5) fs (fromList []) []

complete' lvl fs ditched history = do
  --check what score it is after ditching
  allf <- gets allFeatures
  phiScore <- lift $ gets phi
  score <- lift.lift.phiScore $ diff allf ditched
--  logErr $ "***** SCORE AFTER DITCHING: " ++ show score
  lift $ modify (\st -> st{rejected = ditched})

  stop <- stopAlg fs
  case stop of
    True -> {-logErr "Stopping..." >>-} return (union fs ditched)
    False -> sample lvl ditched fs >>= \res ->
             case res of
               Nothing -> complete' nextLvl fs ditched history
               Just (cs,ds,irred) -> do
                        if elem 1 cs
                         then return (union fs ditched)
                         else do
                           let fs' = diff fs (fromList $ cs++ds)
                               lvl' = (size fs') - 5
                           history' <- calcHistory (union ditched$fromList ds) fs' history
                           printHistory history'
                           complete' lvl' fs' (union ditched (fromList ds)) history'
    where nextLvl = if floor (fromIntegral lvl*1.6) > sz fs
                     then sz fs
                     else floor (fromIntegral lvl*1.6)
          sz = size

calcHistory ditched fs history = do
  phiScore <- lift $ gets phi
  all <- lift $ gets allFeatures
  val <- lift.lift $ phiScore (diff all $ union fs ditched)
  return (history ++ [val])

printHistory hs = log.("Solution history: "++).(++"\n").intercalate " ".map show $ hs

stopAlg :: (RandomGen g,Monad m) => Features -> R g m Bool
stopAlg fs = do
  case size fs of
    0 -> return True
    _ -> score fs >>= return.(==0)

evalAndPart :: (RandomGen g,Monad m) => [Features] -> R g m [Features]
evalAndPart fss = do
  xs <- mapM score fss >>= return.zip fss
  return.map fst.fst.partition ((>0).snd) $ xs

sample :: (RandomGen g,Monad m) => Int -> Features -> Features ->
          R g m (Maybe ([Int],[Int],Features))
sample k ditched fs = do
  --logErr $ "***** LVL NUMBER: " ++ show k
  num <- lift $ gets (numSamples)
  sets <- wrapRand $ sequence (replicate num (randSubset k fs))
  pickElement ditched fs k sets

pickElement :: (RandomGen g, Monad m) => Features -> Features -> Int ->
               [Features] -> R g m (Maybe ([Int],[Int],Features))
pickElement ditched fs lvl fss = do
  s <- firstM (liftM (>0).score) fss
  choose <- lift $ gets chooseElement
  case s of
    (Just f) -> getIrreducible f >>= \irred ->
                lift (lift $ choose irred lvl) >>= \(cs',ds') ->
                (if elem 2 cs' then lift (modify (\st -> st{chooseElement=(\_ _ -> return ([],[]))})) >> return ([],[])
                  else return (cs',ds')) >>= \(cs,ds) ->
                if elem 1 cs
                 then return (Just ([1],[],undefined))
                 else do
                   case length ds == size irred of
                     True -> log "Discarding all\n" >> return (Just ([],ds,irred))
                     False -> 
                         (if null cs
                          then pickMax (union ditched (fromList ds)) fs
                                   (diff irred (fromList ds)) >>= return.(:[])
                          else return cs) >>= \ps ->
                         reportIrreducible irred ps lvl >>
                         return (Just (ps,ds,irred))
    Nothing -> return Nothing

pickMax ditched fs irred = do
  phiScore <- lift (gets phi)
  case size irred of
    1 -> return (head.toList $ irred)
    _ -> do
      current <- lift (gets (flip diff fs.flip diff ditched.allFeatures))
      let iss = split 1 irred
      scores <- mapM (lift.lift.phiScore.union current) $ iss
      log $ "SCORES: " ++ show scores
      return.head.toList.fst.maximumBy (comparing snd).zip iss $ scores

getIrreducible :: (RandomGen g, Monad m) => Features -> R g m Features
getIrreducible fs = getIrreducible' (size fs) fs 0
getIrreducible' startSize fs prog = do
  case size fs of
    1 -> progress prog startSize 1 >> log "\n" >> return fs
    sz -> do
      sub <- search fs
      case sub of
        Nothing -> progress prog startSize sz >>
                   log "\n" >> return fs
        (Just fs') -> progress prog startSize (size fs') >>=
                      getIrreducible' startSize fs'

progress prog startSize curSize = do
  let prog' = floor $
              (fromIntegral (startSize-curSize) / fromIntegral startSize)*60
  sequence_ $ replicate (prog'-prog) (log "=")
  return prog'

log str = do
  inf <- lift (gets info)
  lift (lift $ inf str)

search fs = do
  let sz = size fs
  factor <- lift (gets reductionFactor)
  --logErr $ "Using factor: " ++ show factor
  search' fs sz (floor $ fromIntegral sz / getAvg factor) 1
search' fs sz _ 5 = {-logErr "Number of attempts expired" >>-} return Nothing
search' fs _ 0 _ = {-logErr "Hit 0" >>-} return Nothing
search' fs sz num attemptNum = do
  --logErr $ "Attempt #" ++ show attemptNum ++ "; trying to remove " ++ show num
  lst <- leaveBunchOuts fs (sz - num)
  sub <- firstM (\f -> score f >>= return.(>0)) lst
  let nextNum = let n = num `div` 2
                in if n == 0 then 1 else n
  case sub of
    Nothing -> search' fs sz nextNum (attemptNum+1)
    Just fs' -> --logErr "Succeeded" >>
                adjustFactor sz num >>
                return (Just fs')

adjustFactor sz num = do
  let fac = 0.9 * (fromIntegral sz / fromIntegral num)
  red <- lift (gets reductionFactor)
  lift $ modify (\st -> st{reductionFactor = red `mappend` Avg (1,fac)})

leaveBunchOuts :: (RandomGen g, Monad m) => Features -> Int -> R g m [Features]
leaveBunchOuts fs num = do
  wrapRand.sequence $ replicate 10 (randSubset num fs)

reportIrreducible :: (RandomGen g,Monad m) => Features -> [Int] -> Int -> R g m ()
reportIrreducible fs ps lvl = do
  call <- lift $ gets foundIrreducible
  lift.lift $ call fs ps lvl

wrapRand act = getSplit >>= return.evalRand act

score fs = do
  f <- lift $ gets psi
  rej <- lift $ gets rejected
  lift.lift.f.union rej $ fs

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
