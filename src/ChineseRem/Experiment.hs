{-# LANGUAGE DeriveDataTypeable #-}
module Main
    (
      main
    ) where

import ChineseRem (divisorSearch)
import ChineseRem.Set
import qualified Data.Set as S
import System.Console.CmdArgs

{-
# Test various sizes
# Test various noise rates
-}

data Options = Options
    {
      size :: Int,
      targetSize :: Int,
      noise :: Double,
      reps :: Int
    } deriving (Show,Data,Typeable)

opts = Options
  {
    size = def &= help "Number of elements in problem set (>0)" &= typ "NUM",
    targetSize = def &= help "Number of elements (>0) to place in target" &= typ "NUM",
    noise = def &= help "Probability of noise in sample" &= typ "NUM",
    reps = def &= help "Number of repetitions"
  } &= program "experiment"
    &= summary "Experiment with finding subsets"


data Exp = Exp
    {
      noises :: [Bool]
    , rands :: [Bool]
    , sampleTarget :: S.Set Int
    }

instance Reserved Int where
    reserved = 0

main = do
  options <- cmdArgs opts
  gen <- newStdGen
  let exp = Exp {
              noises = map (<=(1-noise options)) $ randomRs (0,1.0::Double) gen
            , sampleTarget = S.fromList $ take (targetSize options) [1..]
            }
      st = create exp sampler isomorpher (take (size options) [1..])
  return ()

sampler :: Exp -> S.Set Int -> S.Set Int -> (S.Set Int,M Int)
sampler expData idn a =
  let target = sampleTarget expData
      extra = S.toList $ target `S.difference` a
      rem = head.S.toList.S.difference idn $ a
      zeroR = S.size (a `S.intersection` target) > 0
      probs = if null extra then [(reserved,1),(rem,0)]
              else if zeroR then [(reserved,0.5),(rem,0.5)]
                               else [(reserved,0),(rem,1)]
  in noisify expData (M.fromList probs)

noisify dat ps =
  let (p:_,ts) = splitAt 1.noise $ dat
      (u:_,rs) = splitAt 1.rands $ dat
      dat' = dat{rands=rs,noises=ts}
  if p == True
   then return (dat',ps)
   else do
     let scale = if u then 0.2 else 0.3
         ans = normalize.M.map (\p -> scale*p+scale) $ ps
     return (dat',ans)
