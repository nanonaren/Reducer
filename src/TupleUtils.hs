module TupleUtils
    (
      swap
    , ascOrder
    ) where

-- |swap tuple elements
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- |order elements of tuple in ascending order
ascOrder :: Ord a => (a,a) -> (a,a)
ascOrder (a,b) | a <= b = (a,b)
               | otherwise = (b,a)
