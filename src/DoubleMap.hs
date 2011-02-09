{-# LANGUAGE NoMonomorphismRestriction #-}
module DoubleMap
    (
      empty
    , insert
    , filterWithKey
    , fromList
    , toKV
    ) where

import qualified Data.Map as M
import Data.Foldable (foldl')

newtype DoubleMap k v = DM (M.Map k v, M.Map v k)
    deriving (Show)

empty = DM (M.empty,M.empty)

insert k v (DM (m1,m2)) = DM (M.insert k v m1,M.insert v k m2)

filterWithKey f (DM (m1,m2)) =
    DM (M.filterWithKey f m1,M.filterWithKey (flip f) m2)

fromList = foldl' (\m (k,v) -> insert k v m) empty

toKV (DM (m1,m2)) = m1

--runBoth k v f 