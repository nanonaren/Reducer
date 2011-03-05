{-# LANGUAGE TupleSections,TemplateHaskell #-}
module DimensionLearner
    (
      orchestra
    , run
    , Info (..)
    ) where

import SetUtils
import MapUtils (unionWithMonoid)
import ListUtils
import TupleUtils (swap,mapFst)
import Data.List (maximumBy,minimumBy,partition,sort,nubBy)
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
import Text.PrettyPrint

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
        "Global Dep Avg: " ++ show g ++ "\n" ++ showRanking r -- ++ showDeps dm
--        printList "\n" (M.toList dm)

showRanking m = printList "\n" (rsortOn snd lst) ++ "\n"
    where lst = M.toList m

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
          -> S.Set a
          -> St a ()
orchestra np nr s f rem = do
  base <- liftIO.liftM (\v -> 1-v).f $ s
  let useThis = s --S.difference s rem
  -- 1-PARTITION
  parts <- liftIO.fmap (map (S.difference s.S.fromList)).evalRandIO.
           randFixedPartition (S.size useThis) 1.S.toList $ useThis
  partws <- liftIO.mapM f $ parts
  (nonZeroPs,zeroPs) <- processPartition base "1-PARTITION" s parts partws

--  let mini = snd.minimumBy (compare `on` snd) $ nonZeroPs
--      nonZeroPs' = map (\(a,b) -> (a,b-mini)) nonZeroPs
--  orch2 s nonZeroPs zeroPs f
--  allTwos (base+mini) s nonZeroPs' f
  return ()
  --sequence_ (replicate 20 (orch2 s nonZeroPs zeroPs f))

allTwos base s xs f = do
  liftIO.mapM_ (\(x,xv) -> do
                  v <- f (S.difference s x)
                  putStrLn (show (S.toList x) ++ " OLD: " ++ show xv ++ " NEW: " ++ show (1-v-base) ++ " COMP: " ++ comp xv (1-v-base))
               ) $ xss
    where xss = map (mapFst S.fromList).nubBy ((==) `on` fst) $
                [(sort (concat [x,y]),xv+yv) | (x,xv)<-xs,(y,yv)<-xs]

comp a b
    | a == b = "old == new"
    | a > b = "old > new"
    | otherwise = "old < new"

{-
  deps <- liftIO.mapM (\(x,xv) -> do
                         v <- f (S.difference s x)
                         if xv < (1-v)
                          then return x
                          else return (S.empty)
                      ) $ xss
  liftIO.putStrLn.render.fsep.punctuate comma.map (text.show.S.toList).filter ((>1).S.length) $ deps -}

{-
orch2 s nonZeroPs zeroPs f = do
  parts2 <- liftIO.fmap (map (S.difference s.S.fromList)).evalRandIO.
            randFixedPartition (length nonZeroPs) 2 $ nonZeroPs
  partws2 <- liftIO.mapM f $ parts2
  processPartition "2-PARTITION" s parts2 partws2
  return ()
-}
--  parts <- liftIO.evalRandIO.fmap (map (S.difference s).concat) $ 
--            mapM (\_ -> randPartition np s) [1..nr]
--  partws <- liftIO.mapM f $ parts
--  mapM_ (\(pw,p) -> subOrchestra s f pw p).zip partws $ parts
--  normalizeDeps

--partIt s = 

processPartition :: (Ord a,Show a) => Double -> String -> S.Set a -> [S.Set a]
                 -> [Double] -> St a ([([a],Double)],[([a],Double)])
processPartition base name s ps pws = do
  let doc = nameIt name $+$
            nest 5 (vcat [cLength,cTotalUncapped,convergenceSeries,allPartOrder])
  liftIO $ putStrLn.render $ doc
  return (map swap nonZeroPs,map swap zeroPs)
    where ordered = rsortOn fst.zip (map (\v -> 1-v-base) pws).
                    map (S.toList.S.difference s) $ ps
          (nonZeroPs,zeroPs) = partition ((/=0).fst) ordered
          allPartOrder = (nameIt "Partition Order"<>).fsep.punctuate comma.
                         map (text.show.snd) $ nonZeroPs
          cLength = (nameIt "Convergence Length" <>).int.sum'.
                    map (length.snd) $ nonZeroPs
          cTotalUncapped = (nameIt "Uncapped Total" <>).double
                           .sumBy' fst $ nonZeroPs
          convergenceSeries = (nameIt "Convergence sequence"<>).fsep.
                              punctuate comma.map double.scanl (+) 0.
                              map (fst) $ nonZeroPs
          nameIt nm = text nm <> colon <> space

subOrchestra :: (Ord a,Show a) => S.Set a -> (S.Set a -> IO Double)
             -> Double -> S.Set a -> St a ()
subOrchestra s f partw part = do
  update (S.difference s part) (1-partw)
  -- cofacs <- liftIO.evalRandIO $ randCoprimeFactors s part
  -- case cofacs of
  --   Nothing -> return () --liftIO.putStrLn.show $ "No factors to look at"
  --   Just (f1,f2) -> do
  --               update (S.difference s part) (1-partw)
                -- f1w <- liftIO.f $ f1
                -- f2w <- liftIO.f $ f2
                -- let diff = (1-partw) - ((1-f1w)+(1-f2w))
                -- updateDeps (S.difference s part) (1-partw) (S.difference s f1)
                --            (S.difference s f2) diff
                -- liftIO.putStrLn $ "DIVISOR: " ++ show part
                -- liftIO.putStrLn $ "DIVISOR REM WEIGHT: " ++ show (1-partw)
                -- liftIO.putStrLn $ "F1 REM WEIGHT: " ++ show (1-f1w)
                -- liftIO.putStrLn $ "F2 REM WEIGHT: " ++ show (1-f2w)
                -- liftIO.putStrLn.show $ diff

update r rw = do
  T.modify (mappend (Info M.empty mempty ranks))
    where ranks = M.fromList.map (,Avg 1 weight).S.toList $ r
          weight = rw / fromIntegral (S.size r)

updateDeps :: Ord a => S.Set a -> Double -> S.Set a
           -> S.Set a -> Double -> St a ()
updateDeps r rw r1 r2 w = do
  T.modify (mappend (Info deps (Avg 1 w) ranks))
    where pairs = chooseSortedPairs (S.toList r1) (S.toList r2)
          deps = M.fromList.map (,Avg 1 w) $ pairs
          ranks = M.fromList.map (,Avg 1 rw).S.toList $ r

normalizeDeps :: St a ()
normalizeDeps = do
  st <- T.get
  let global = globalDepAvg_ st
      deps = M.map (\x -> (avg^=avg_ x - avg_ global) x) (depMap_ st)
  T.modify (depMap^=deps)
