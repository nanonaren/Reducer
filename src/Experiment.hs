{-# LANGUAGE DeriveDataTypeable,TupleSections #-}
module Main
    (
      main
    ) where

import ChineseRem (divisorSearch2)
import ChineseRem.Set (run)
import ChineseRem.IndepSet
import ListUtils (rsortOn)

import Control.Monad.State
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
      reps :: Int,
      sampleLevel :: Int
    } deriving (Show,Data,Typeable)

opts = Options
  {
    size = def &= help "Number of elements in problem set (>0)" &= typ "NUM",
    targetSize = def &= help "Number of elements (>0) to place in target" &= typ "NUM",
    noise = def &= help "Probability of noise in sample" &= typ "NUM",
    reps = def &= help "Number of repetitions",
    sampleLevel = def &= help "Number of levels (from bottom) to sample" &= typ "NUM"
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

type St = State Exp

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
  st <- create sampler (sampleLevel options) (take (size options) [1..])
  let res = flip evalState exp.run (searchSet (reps options)) $ st
  printResults target res

searchSet n = do
  dss <- sequence (replicate n $ divisorSearch2 (S.fromList []))
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

sampler :: S.Set Int -> S.Set Int -> St (MDist Int)
sampler idn a = do
  target <- gets sampleTarget
  let rems = S.difference idn a
      remList = S.toList rems
      tsize = fromIntegral.S.size $ target
      (p,divProbs) = aprobs target a
      shared = (1-p)/fromIntegral (S.size rems)
      probs = divProbs ++ map (,shared) remList
  noisify (M.fromList probs)

aprobs target a = (p,map f.S.toList $ a)
    where tsize = fromIntegral.S.size $ target
          p = interSize/tsize
          inter = S.intersection target $ a
          interSize = fromIntegral (S.size inter)
--          shared = p / interSize
          shared = p / fromIntegral (S.size a)
--          f x = if S.member x inter then (x,shared) else (x,0)
          f x = (x,shared)

noisify ps = do
  (p:_,ts) <- gets (splitAt 1.noises)
  (u:_,rs) <- gets (splitAt 1.rands)
  modify (\s -> s{rands=rs,noises=ts})
  return $ if p == True
           then ps
           else
               let ans = M.fromList.uncurry zip.fmap (switch u).
                         unzip.M.toList $ ps
               in ans

switch True (0.99:0.01:_) = [0.4,0.6]
switch False (0.99:0.01:_) = [0.4,0.6]
switch True _ = [0.6,0.4]
switch False _ = [0.6,0.4]
