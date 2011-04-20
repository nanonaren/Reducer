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

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

type MDist a = M.Map a Double

class Reserved a where
    reserved :: a

--instance Reserved Int where
--    reserved = 0

type SetFinderIndep m u a = C.SetFinder m u a (MDist a)

isomorph :: (Monad m,Ord a,Reserved a) => (S.Set a,MDist a) ->
            (S.Set a,MDist a) -> m (MDist a)
isomorph (a,da) (b,db) = do
  return (combine da db)
{-
  let zeroA = getZero da
      zeroB = getZero db
      final = normalize $ M.insert reserved (zeroA*zeroB) (M.union da db)
  return final
-}

combine :: Ord a => MDist a -> MDist a -> MDist a
combine a = M.map (/2).M.unionWith (+) a

getZero m =
    case M.lookup reserved m of
      Nothing -> 0
      Just x -> x

create uSam ulvl elems = C.create uSam isomorph combine ulvl elems