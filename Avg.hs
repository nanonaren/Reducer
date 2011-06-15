module Avg
    (
      Avg (..)
    , getAvg
    ) where

import Data.Monoid

newtype Avg a = Avg (a,a)
    deriving (Show)

instance (Num a,Fractional a) => Monoid (Avg a) where
    mempty = Avg (0,0)
    (Avg (c1,v1)) `mappend` (Avg (c2,v2)) = Avg (c1+c2,(c1*v1+c2*v2)/(c1+c2))

getAvg (Avg (_,v)) = v
