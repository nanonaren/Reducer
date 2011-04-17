module RandUtils
    (
      chooseInt
    ) where

chooseInt :: Integral a => Double -> a -> a -> a
chooseInt p minB maxB = minB + floor (p*len)
    where len = fromIntegral $ (maxB - minB) + 1