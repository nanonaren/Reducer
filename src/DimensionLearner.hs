{-# LANGUAGE TupleSections,TemplateHaskell #-}
module DimensionLearner
    (
      orchestra
    , run
    , Info (..)
    ) where

import SetUtils
import MapUtils (unionWithMonoid)
import ListUtils (chooseSortedPairs,printList)
import Data.List (maximumBy,minimumBy)
import Data.Function (on)
import Control.Monad.Random
import Control.Monad.State as T
import qualified Data.Set as S
import qualified Data.Map as M
import qualified DoubleMap as D
import Data.Monoid
import Data.Accessor.Template
import Data.Accessor.Basic
import System.Random

data Avg = Avg
    {
      avgCount_ :: Double,
      avg_ :: Double
    }

instance Show Avg where
    show (Avg c a) = show (c,a)

instance Eq Avg where
    (Avg c1 v1) == (Avg c2 v2) = v1 == v2

instance Ord Avg where
    (Avg c1 v1) <= (Avg c2 v2) = v1 <= v2

instance Monoid Avg where
    mempty = Avg 0 0
    (Avg n1 a1) `mappend` (Avg n2 a2) = n `seq` a `seq` Avg n a
        where n = n1 + n2
              a = (n1*a1 + n2*a2)/(n1+n2)

data Info a = Info
    {
      depMap_ :: M.Map (a,a) Avg
    , globalDepAvg_ :: Avg
    , ranking_ :: M.Map a Avg
    }

instance Show a => Show (Info a) where
    show (Info dm g r) =
        "Global Dep Avg: " ++ show g ++ "\n" ++ showDeps dm
--        printList "\n" (M.toList dm)

showDeps m =
    "Min Dep Value: " ++ show (minimumBy (compare `on` snd) lst) ++ "\n" ++
    "Max Dep Value: " ++ show (maximumBy (compare `on` snd) lst) ++ "\n"
    where lst = M.toList m

--Derive accessors
$( deriveAccessors ''Info )
$( deriveAccessors ''Avg )

instance Ord a => Monoid (Info a) where
    mempty = Info M.empty mempty M.empty
    (Info d1 g1 r1) `mappend` (Info d2 g2 r2) =
        Info (unionWithMonoid d1 d2) (g1 `mappend` g2)
             (unionWithMonoid r1 r2)

type St a = T.StateT (Info a) IO

run m = execStateT m emptyInfo

emptyInfo = Info M.empty mempty M.empty

-- |Does stuff
orchestra :: (Ord a,Show a) =>
             Int -- ^Number of partitions
          -> Int -- ^Number of repetitions
          -> S.Set a -- ^The set of features
          -> (S.Set a -> IO Double) -- ^The objective function
          -> St a ()
orchestra np nr s f = do
  parts <- liftIO.evalRandIO.fmap (map (S.difference s).concat) $ 
           mapM (\_ -> randPartition np s) [1..nr]
  partws <- liftIO.mapM f $ parts
  mapM_ (\(pw,p) -> subOrchestra s f pw p).zip partws $ parts
  normalizeDeps

subOrchestra :: (Ord a,Show a) => S.Set a -> (S.Set a -> IO Double)
             -> Double -> S.Set a -> St a ()
subOrchestra s f partw part = do
  cofacs <- liftIO.evalRandIO $ randCoprimeFactors s part
  case cofacs of
    Nothing -> return () --liftIO.putStrLn.show $ "No factors to look at"
    Just (f1,f2) -> do
                f1w <- liftIO.f $ f1
                f2w <- liftIO.f $ f2
                let diff = (1-partw) - ((1-f1w)+(1-f2w))
                updateDeps (S.difference s f1) (S.difference s f2) diff
                -- liftIO.putStrLn $ "DIVISOR: " ++ show part
                -- liftIO.putStrLn $ "DIVISOR REM WEIGHT: " ++ show (1-partw)
                -- liftIO.putStrLn $ "F1 REM WEIGHT: " ++ show (1-f1w)
                -- liftIO.putStrLn $ "F2 REM WEIGHT: " ++ show (1-f2w)
                -- liftIO.putStrLn.show $ diff

updateDeps :: Ord a => S.Set a -> S.Set a -> Double -> St a ()
updateDeps r1 r2 w = do
  T.modify (mappend (Info deps (Avg 1 w) M.empty))
    where pairs = chooseSortedPairs (S.toList r1) (S.toList r2)
          deps = M.fromList.map (,Avg 1 w) $ pairs

normalizeDeps :: St a ()
normalizeDeps = do
  st <- T.get
  let global = globalDepAvg_ st
      deps = M.map (\x -> (avg^=avg_ x - avg_ global) x) (depMap_ st)
  T.modify (depMap^=deps)
