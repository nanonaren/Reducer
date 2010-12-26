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
      extra = S.toList $ target `S.difference` a
      rem = head.S.toList.S.difference idn $ a
      zeroR = S.size (a `S.intersection` target) > 0
      probs = if null extra then [(reserved,1),(rem,0)]
              else if zeroR then [(reserved,0.5),(rem,0.5)]
                               else [(reserved,0),(rem,1)]
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

switch True (1:0:_) = [0.5,0.5]
switch False (1:0:_) = [0.5,0.5]
switch True (0:1:_) = [0.5,0.5]
switch False (0:1:_) = [0.5,0.5]
switch True (0.5:0.5:_) = [0.7,0.3]
switch False (0.5:0.5:_) = [0.3,0.7]
switch _ _ = error "MADNESS"
