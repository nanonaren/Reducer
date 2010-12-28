{-# LANGUAGE DeriveDataTypeable #-}
module Main
    (
      main
    ) where

import ChineseRem (divisorSearch)
import ChineseRem.Set (run)
import ChineseRem.IndepSet
import ListUtils (rsortOn)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.CmdArgs
import System.Random
import Text.Printf

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
  gen2 <- newStdGen
  let exp = Exp {
              noises = map (<=(1-noise options)) $ randomRs (0,1.0::Double) gen
            , rands = randomRs (False,True) gen2
            , sampleTarget = target
            }
      target = S.fromList $ take (targetSize options) [1..]
  st <- create exp sampler (take (size options) [1..])
  let res = run (searchSet (reps options)) $ st
  printResults target res

searchSet n = do
  dss <- sequence (replicate n $ divisorSearch (S.fromList []))
  return.foldl' (M.unionWith (+)) M.empty $ dss

printResults expected actual = do
  putStrLn.show $ expected
  putStrLn.show $ actual
  let actual' = S.fromList.map fst.take (S.size expected).rsortOn snd.
                M.toList.M.delete reserved $ actual
      missing = S.size.S.difference expected $ actual'
      extra = S.size.S.difference actual' $ expected
  printf "Number of missing elements: %i\n\
         \Number of extra elements: %i\n"
         missing extra

sampler :: Exp -> S.Set Int -> S.Set Int -> (Exp,MDist Int)
sampler expData idn a =
  let target = sampleTarget expData
      rem = head.S.toList.S.difference idn $ a
      remRequired = S.member rem target
      probs = if not remRequired then [(reserved,0.99),(rem,0.01)]
              else [(reserved,0.01),(rem,0.99)]
--              else if zeroR then [(reserved,bothProb),(rem,1-bothProb)]
--              else [(reserved,0),(rem,1)]
--      tsize = fromIntegral.S.size $ target
--      bothProb = (tsize - 1) / tsize
  in noisify expData (M.fromList probs)

noisify dat ps =
  let (p:_,ts) = splitAt 1.noises $ dat
      (u:_,rs) = splitAt 1.rands $ dat
      dat' = dat{rands=rs,noises=ts}
  in if p == True
     then (dat',ps)
     else
         let ans = M.fromList.uncurry zip.fmap (switch u).
                   unzip.M.toList $ ps
         in (dat',ans)

switch True (0.99:0.01:_) = [0.4,0.6]
switch False (0.99:0.01:_) = [0.4,0.6]
switch True _ = [0.6,0.4]
switch False _ = [0.6,0.4]
