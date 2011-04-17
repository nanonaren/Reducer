module ContainerUtils
    (
      normalize
    ) where

import Data.Foldable

normalize :: (Foldable t,Functor t,Fractional a) => t a -> t a
normalize t = let total = foldl' (+) 0 t
              in fmap (/total) t