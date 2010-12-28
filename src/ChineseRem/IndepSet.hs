module ChineseRem.IndepSet
    (
      SetFinderIndep
    , Reserved (..)
    , MDist
    , create
    , normalize
    ) where

import qualified ChineseRem.Set as C
import ContainerUtils (normalize)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type MDist a = M.Map a Double

class Reserved a where
    reserved :: a

type SetFinderIndep u a = C.SetFinder u a (MDist a)

isomorph :: (Ord a,Reserved a) => (S.Set a,MDist a) -> (S.Set a,MDist a) -> MDist a
isomorph (a,da) (b,db) =
  let zeroA = getZero da
      zeroB = getZero db
      final = normalize $ M.insert reserved (zeroA*zeroB) (M.union da db)
  in final

getZero m =
    case M.lookup reserved m of
      Nothing -> 0
      Just x -> x

create uData uSam elems = C.create uData uSam iso elems
    where iso u x y = (u,isomorph x y)
