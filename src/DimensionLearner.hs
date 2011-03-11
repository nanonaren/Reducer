{-# LANGUAGE TupleSections,TemplateHaskell #-}
module DimensionLearner
    (
      orchestra
    , run
    , Info (..)
    ) where

import SetUtils
import MapUtils (unionWithMonoid)
import NanoUtils.List
import NanoUtils.Monadic.List
import TupleUtils (swap,mapFst)
import Data.List (maximumBy,minimumBy,partition,sort,nubBy,nub,foldl')
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
import Text.PrettyPrint hiding ((<+>))

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
    , baseScore_ :: Double
    , scoref_ :: S.Set a -> IO Double
    , fSet_ :: S.Set a
    }

instance Show a => Show (Info a) where
    show (Info dm g r _ _ _) =
        "Global Dep Avg: " ++ show g ++ "\n" ++ showRanking r -- ++ showDeps dm
--        printList "\n" (M.toList dm)

data Part a = Part
    {
      part :: S.Set a
    , score :: Double
    , softClosure :: Bool
    }

instance Show a => Show (Part a) where
    show (Part p s b) = show (S.toList p,s,b)

instance Eq a => Eq (Part a) where
    (Part _ x _) == (Part _ y _) = x == y

instance Ord a => Ord (Part a) where
    compare (Part _ x _) (Part _ y _)
        | x == y = EQ
        | x < y = LT
        | otherwise = GT

showRanking m = printList "\n" (rsortOn snd lst) ++ "\n"
    where lst = M.toList m

showDeps m =
    "Min Dep Value: " ++ show (minimumBy (compare `on` snd) lst) ++ "\n" ++
    "Max Dep Value: " ++ show (maximumBy (compare `on` snd) lst) ++ "\n"
    where lst = M.toList m

--Derive accessors
$( deriveAccessors ''Info )
$( deriveAccessors ''Avg )

-- instance Ord a => Monoid (Info a) where
--     mempty = Info M.empty mempty M.empty undefined undefined 
--     (Info d1 g1 r1) `mappend` (Info d2 g2 r2) =
--         Info (unionWithMonoid d1 d2) (g1 `mappend` g2)
--              (unionWithMonoid r1 r2)

type St a = T.StateT (Info a) IO

run m = execStateT m emptyInfo

emptyInfo = Info M.empty mempty M.empty undefined undefined undefined

a@(Part p1 s1 _) <+> b@(Part p2 s2 _)
    | S.null p1 = b
    | S.null p2 = a
    | otherwise = Part (S.union p1 p2) (s1+s2) True

a@(Part p1 s1 b1) <++> b@(Part p2 s2 b2)
    | S.null p1 = return b
    | S.null p2 = return a
    | otherwise = do
  base <- gets (baseScore_)
  s <- gets (fSet_)
  f <- gets (scoref_)
  let p3 = S.union p1 p2
  s3 <- liftIO.liftM (base-) $ f (S.difference s p3)
  let bc = not (not b1 && not b2 && (s1+s2 < s3))
  return (Part p3 s3 bc)

emptyPart = Part S.empty 0 False

concatParts [] = return emptyPart
concatParts (x:xs) = do
  let pt = concatParts' xs
  x <++> pt

concatParts' :: Ord a => [Part a] -> Part a
concatParts' = foldl' (<+>) emptyPart

markAsNotSoft (Part p1 s1 _) = Part p1 s1 False

sortParts = rsortOn score

stateSetup s f = do
  base <- liftIO.f $ s
  T.modify (baseScore^=base)
  T.modify (scoref^=f)
  T.modify (fSet^=s)

-- |Does stuff
orchestra :: (Ord a,Show a) =>
             S.Set a -- ^The set of features
          -> (S.Set a -> IO Double) -- ^The objective function
          -> a
          -> St a ()
orchestra s f r = do
  stateSetup s f
  base <- gets (baseScore_)
  let useThis = s --S.difference s rem
  -- 1-PARTITION
  parts <- liftIO.fmap (map (S.difference s.S.fromList)).evalRandIO.
           randFixedPartition (S.size useThis) 1.S.toList $ useThis
  partws <- liftIO.mapM f $ parts
  (nonZeroPs,zeroPs) <- processPartition base "1-PARTITION" s parts partws
--  val <- liftIO $ f (S.difference s (S.fromList.concat $ map fst zeroPs))
--  liftIO.putStrLn.show $ val
--  let mini = snd.minimumBy (compare `on` snd) $ nonZeroPs
--      nonZeroPs' = map (\(a,b) -> (a,b-mini)) nonZeroPs
--  orch2 s nonZeroPs zeroPs f
  -- liftIO.putStrLn $ "#####NON-ZEROS#####"
  -- xss <- allTwos base s nonZeroPs f
  -- prepareNextLevel xss

  -- liftIO.putStrLn $ "#####ZEROS#####"
  -- xss <- allTwos base s zeroPs f
  -- prepareNextLevel xss
  let ps = map (\(xs,v) -> Part (S.fromList xs) v False) zeroPs
      pset = S.fromList.concat $ map fst zeroPs
  pscore <- liftIO $ f (S.difference s pset)
  stuff <- factorIrreducibles (Part pset (base - pscore) True)
  liftIO.putStrLn.show $ stuff

  -- let pset' = S.difference pset ps
  -- pscore' <- liftIO $ f (S.difference s pset')
  -- stuff2 <- mad (Part pset' (base - pscore') True)
  -- liftIO.putStrLn.show $ stuff2
--  exploreAll ps

  return ()
  --sequence_ (replicate 20 (orch2 s nonZeroPs zeroPs f))

prepareNextLevel xss = do
  let mp = foldl' arrange M.empty xss
  liftIO.putStrLn.printList "\n".M.toList.M.map collapse $ mp
    where arrange m xs =
              foldl' (\m x -> case M.lookup x m of
                                Nothing -> M.insert x [xs] m
                                Just _ -> M.insertWith (++) x [xs] m) m.fst $ xs
          collapse = nub.concat.map fst.filter ((==GT).snd)

exploreAll ps = do
  if length ps == 1
   then return ps
   else directPartition ps >>= exploreAll

directPartition ps = do
  ps' <- eqClassesTM relation ps >>=
         return.partition ((==1).length) >>= \(lvlParts,others) ->
         mapM concatParts others >>= \others' ->
         return (map concatParts' (pairUp (concat lvlParts)) ++ others')
  liftIO.putStrLn.show.sortParts $ ps'
  return ps'
    where relation p1 p2 = liftM2 (<) (return (p1 <+> p2)) (p1 <++> p2)

factorIrreducibles prt@(Part ps s b) = do
  base <- gets (baseScore_)
  iden <- gets (fSet_)
  f <- gets (scoref_)
  factor <- factorIrreducible prt
  case ps == part factor of
    True -> return [factor]
    False -> do
      let ps' = S.difference ps (part factor)
      s' <- liftM (base-).liftIO.f.S.difference iden $ ps'
      liftM (factor:) $ factorIrreducibles (Part ps' s' True)

factorIrreducible part@(Part _ 0 _) = return part
factorIrreducible part@(Part ps s b) = do
  liftIO.putStrLn $ "START: " ++ show s
  base <- gets (baseScore_)
  iden <- gets (fSet_)
  f <- gets (scoref_)
  (Part ps' _ _) <- wittle part
  liftIO.putStrLn.show $ S.difference ps ps'
  (ps'',s') <- deleteOne f base iden (S.toList ps') ps'
  case ps' == ps'' of
    True -> liftIO (putStrLn $ "END: " ++ show s) >> return (Part ps' s False)
    False -> liftIO (putStrLn $ "END: " ++ show s') >> factorIrreducible (Part ps'' s' b)

deleteOne _ _ _ [] original = return (original,0)
deleteOne f base iden (p:ps) original = do
  let test = S.delete p original
  s <- liftM (base-).liftIO.f.S.difference iden $ test
  case s == 0 of
    True -> deleteOne f base iden ps original
    False -> return (test,s)

wittle (Part ps s b) = do
  f <- gets (scoref_)
  iden <- gets (fSet_)
  base <- gets (baseScore_)
  ps' <- wittle' base iden f (map (,False).S.toList $ ps) s
  return (Part (S.fromList ps') s b)
wittle' _ _ _ [] _ = error "Cannot have null list when wittling."
wittle' _ _ _ lst@((_,True):ps) _ = return (map fst lst)
wittle' base iden f ((p,_):ps) s = do
  val <- liftIO.f.S.difference iden.S.fromList.map fst $ ps
  case (base-val) == s of
    True -> liftIO (putStrLn $ "removed: " ++ show p) >> wittle' base iden f ps s
    False -> wittle' base iden f (ps ++ [(p,True)]) s

allTwos base s xs f = do
  liftIO.mapM (\(x,xv) -> do
                  v <- f (S.difference s x)
                  return (S.toList x,compare xv (base-v))
--                  when (xv < (base-v)) $ putStrLn (show (S.toList x) ++ " OLD: " ++ show xv ++ " NEW: " ++ show (base-v) ++ " COMP: " ++ comp xv (base-v))
               ) $ xss
    where xss = map (mapFst S.fromList).filter ((>1).length.fst).
                nubBy ((==) `on` fst) $
                [(sort.nub $ (concat [x,y]),xv+yv) | (x,xv)<-xs,(y,yv)<-xs]

comp a b
    | a == b = "old == new"
    | a > b = "old > new"
    | otherwise = "old < new"

processPartition :: (Ord a,Show a) => Double -> String -> S.Set a -> [S.Set a]
                 -> [Double] -> St a ([([a],Double)],[([a],Double)])
processPartition base name s ps pws = do
  let doc = nameIt name $+$
            nest 5 (vcat [cLength,cTotalUncapped,convergenceSeries,allPartOrder])
  --liftIO $ putStrLn.render $ doc
  return (map swap nonZeroPs,map swap zeroPs)
    where ordered = rsortOn fst.zip (map (\v -> base-v) pws).
                    map (S.toList.S.difference s) $ ps
          (nonZeroPs,zeroPs) = partition ((>0).fst) ordered
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

