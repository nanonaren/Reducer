module MapUtils
    (
      topWithKey
    , unionWithMonoid
    ) where

import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Monoid

topWithKey :: M.Map k a -> Int -> [(k,a)]
topWithKey mp n = take n $ unfoldr M.maxViewWithKey mp

unionWithMonoid :: (Ord k,Monoid a) => M.Map k a -> M.Map k a -> M.Map k a
unionWithMonoid = M.unionWith mappend