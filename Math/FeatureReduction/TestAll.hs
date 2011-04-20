module Main
    (
      main
    ) where

import Math.FeatureReduction.TFeatures
import System.Exit

main = do
  b <- testFeatures
  case b of
    True -> exitSuccess
    False -> exitFailure
  
