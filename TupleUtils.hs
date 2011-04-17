module TupleUtils
    (
      swap
    , ascOrder
    , mapFst
    ) where

-- |swap tuple elements
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- |apply function to the first element of tuple
mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a,b)

-- |order elements of tuple in ascending order
ascOrder :: Ord a => (a,a) -> (a,a)
ascOrder (a,b) | a <= b = (a,b)
               | otherwise = (b,a)
