module Main
    (
      main
    ) where

import Math.FeatureReduction.TFeatures
import Math.FeatureReduction.TBase
import Test.Framework (defaultMain, testGroup)

main = defaultMain tests

tests = testFeatures ++ testBase
