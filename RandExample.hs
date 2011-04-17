module Main
    (
      main
    ) where

import Data.Random
import Data.Random.RVar

normalPair =  do
  u <- stdUniform
  t <- stdUniform
  let r = sqrt (-2 * log u)
      theta = (2 * pi) * t
      x = r * cos theta
      y = r * sin theta
      return (x,y)
