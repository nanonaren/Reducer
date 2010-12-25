{-# LANGUAGE DeriveDataTypeable #-}
module Main
    (
      main
    ) where

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

main = do
  options <- cmdArgs opts

