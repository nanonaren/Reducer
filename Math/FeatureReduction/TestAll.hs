module Main
    (
      main
    ) where

import Math.FeatureReduction.TFeatures
import Math.FeatureReduction.TBase
import System.Exit

main = do
  b <- fmap and $ sequence [testFeatures,testBase]
  case b of
    True -> exitSuccess
    False -> exitFailure

