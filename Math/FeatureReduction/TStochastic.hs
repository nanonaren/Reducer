module Math.FeatureReduction.TStochastic
    (
    ) where

import Math.FeatureReduction.Features
import Math.FeatureReduction.Stochastic
import System.Random
import System.IO
import NanoUtils.Set (randPicks)
import Control.Monad.Random (evalRandIO)
import Data.List (nub)

main = do
  hSetBuffering stdout NoBuffering
  gen <- newStdGen
  let xs = [1..100]
      fs = fromList xs
      phi = samplePhi 692
  fs' <- runR fs phi 20 (\f i lvl -> putStrLn $ show lvl ++ " : " ++ show (size f)) 692 fs gen -- ++ " : " ++ show f ++ " : " ++ show i) 692 fs gen
  let sub = diff fs fs'
  val <- phi sub
  putStrLn $ show val ++ " : " ++ show sub

--main = do
--  hSetBuffering stdout NoBuffering
--  gen <- newStdGen
--  let xs = nub.take 50 $ randomRs (1,10000) gen
--      fs = fromList xs
--      phi = samplePhi2 10000
--  val <- phi fs
--  interactive2 phi fs fs
--  interactive phi fs val val 20000 fs

interactive2 phi all fs = do
  target <- phi fs
  putStrLn $ "FEATURES: " ++ show fs ++ "; TARGET: " ++ show target
  gen <- newStdGen
  fs' <- runR fs phi 30 (\f i lvl -> putStrLn $ show lvl ++ " : " ++ show f ++ " : " ++ show i) target fs gen
  let sub = diff all fs'
  val <- phi sub
  putStrLn $ show val ++ " : " ++ show sub
  (sub',val') <- pickTillDrop phi val sub fs'
  putStrLn $ show val' ++ " : " ++ show sub'
  putStrLn "continue?"
  hGetLine stdin
  interactive2 phi all sub'

pickTillDrop phi maxVal core fs = do
  case size fs of
    0 -> return (core,maxVal)
    _ -> do
      (rest,ps) <- evalRandIO $ randPicks (size fs) 2 (toList fs)
      let core' = union core (fromList ps)
          fs' = fromList rest
      val <- phi core'
      case val > maxVal of
        True -> return (core',val) -- pickTillDrop phi val core' fs'
        False -> return (core',val)

interactive phi all low target high fs = do
  putStrLn $ "LOW: " ++ show low ++ "; HIGH: " ++ show high ++
             "; CURRENT: " ++ show target ++ "; FEATURES: " ++ show fs
  gen <- newStdGen
  fs' <- runR all phi 10 (\f i lvl -> putStrLn $ show lvl ++ " : " ++ show f ++ " : " ++ show i) target fs gen
  let sub = diff all fs'
  val <- phi sub
  putStrLn $ show val ++ " : " ++ show sub
  putStr "Go higher? [y/n] "
  c <- hGetLine stdin
  putStrLn ""
  case c of
    "y" -> let target' = (target + high)/2
           in interactive phi all target target' high fs
    "d" -> return ()
    _   -> let target' = (low + target)/2
           in interactive phi all low target' target fs

samplePhi :: Double -> Features -> IO Double
samplePhi n = return.pick.fromIntegral.sum.toList
    where pick x | x > n = n
                 | otherwise = x

samplePhi2 n = return.fromIntegral.penalize.sum.toList
    where pick x | x > n = n
                 | otherwise = x
          penalize v = if v-n > 0
                       then if v-n > n then 0 else n - (v-n)
                       else v