module ChineseRem.IndepSet
    (
      SetFinderIndep
    , Reserved
    , create
    , normalize
    ) where

import qualified ChineseRem.Set as C
import ContainerUtils (normalize)
import qualified Data.Map as M

type MDist a = M.Map a Double

class Reserved a where
    reserved :: a

type SetFinderIndep u a = C.SetFinder u a (MDist a)

isomorph :: (Ord a,Reserved a) => MDist a -> MDist a -> MDist a
isomorph a b =
  let zeroA = getZero a
      zeroB = getZero b
      a' = M.map (zeroB*) a
      b' = M.map (zeroA*) b
      final = normalize $ M.insert reserved (zeroA*zeroB) (M.union a' b')
  in final

getZero m =
    case M.lookup reserved m of
      Nothing -> 0
      Just x -> x

create uData uSam elems = C.create uData uSam iso elems
    where iso u (a,da) (b,db) = (u,isomorph da db)
