{-# LANGUAGE NoMonomorphismRestriction,TupleSections #-}
module Main
    (
      main
    ) where

import LCASt
import NNLS
import Math.FeatureReduction.Base
import Math.FeatureReduction.Features
import NanoUtils.List (rsortOn,sortOn)
import NanoUtils.Tuple (swap)

import Control.Monad.State
import Data.Maybe (fromJust,isNothing)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.CmdArgs
import System.IO
import System.Random
import System.Random.Shuffle (shuffle')

import qualified Network.Memcache as C
import Network.Memcache.Protocol
import Pipes

sampleState = FeatureInfo
  {
    allFS = fromList [1..75]
  , workingSet = fromList []
  , phi = myPhi
  , psi = phiToPsi myPhi
  , foundIrreducible = myFoundIrreducible
  , info = myInfo
  }

myInfo = liftIO.putStrLn

main = do
  args <- cmdArgs opts
  let lca' = lca{options = args}
--  val <- evalStateT (setupCache >> setupR >> setupNodes >>
--                     setupFeatures >> fromNodeNames [271,364,1844,308,1171,7218,9272] >>= \f1 ->
--                     fromNodeNames [271,1110,364,1844,6652,308,693] >>= \f2 ->
--                     myPhiMap (f1:f2:[])) lca'
--  print val
  fs <- evalStateT (setupCache >> setupR >> setupNodes >>
                    setupFeatures >> runReducer complete sampleState >>=
                    toNodeNames) lca'
  print fs


setupCache :: St ()
setupCache = do
  st <- get
  srvr <- liftIO $ connect "localhost" 11211
  put st{server = srvr}

setupNodes :: St ()
setupNodes = do
  st <- gets options
  allnms <- fmap (M.fromList.flip zip [1..].map (head.words).lines) $
            liftIO (readFile (namesFile st))
  mustInc <- fmap (map (head.words).lines) $
             liftIO (readFile (mustInclude st))
  --select required random nodes
  gen <- liftIO newStdGen
  let notRequired = M.keys allnms \\ mustInc
      randNodes = take (numRandomNodes st) $
                  shuffle' notRequired (length notRequired) gen
      names = M.filterWithKey (\k _ -> elem k mustInc || elem k randNodes) allnms
      revNames = M.fromList.map swap.M.toList $ names
      rootNum = fromJust.M.lookup (rootNode st) $ names
      names' = M.delete (rootNode st) names
      revNames' = M.delete rootNum revNames
  modify (\s -> s{names=names',revNames=revNames',root=rootNum})

-- Call only after setupNodes
setupFeatures :: St ()
setupFeatures = do
  st <- get
  ks <- gets (M.keys.revNames)
  let fToLn = M.fromList.zip [1..] $ ks
      lnToF = M.fromList.map swap.M.toList $ fToLn
  put st{toFeatures=fromList.map (fromJust.flip M.lookup lnToF),
         fromFeatures=map (fromJust.flip M.lookup fToLn).toList}

fromNodeNames :: [Int] -> St Features
fromNodeNames xs = do
  nmap <- gets names
  f <- gets toFeatures
  return.f.map (fromJust.flip M.lookup nmap) $ map show xs

toNodeNames :: Features -> St [Int]
toNodeNames fs = do
  lns <- gets (($fs).fromFeatures)
  revN <- gets revNames
  return $ map (read.fromJust.flip M.lookup revN) lns

myPhiMap :: [Features] -> St [Double]
myPhiMap fss = do
  root <- gets root
  fromFS <- gets fromFeatures
  (other,cached) <- fmap (partition (isNothing.snd)).
                    mapM (\(i,fs) -> lookupCache fs >>= return.((i,fs),)).
                    zip [1..] $ fss
  let (ids,xss) = unzip.fst.unzip $ other
      cached' = map (\((i,_),v) -> (i,fromJust v)) cached
  vals <- fmap (fst.unzip).nnlsMap root.map fromFS $ xss
  mapM_ (uncurry putInCache) $ zip xss vals
  return.snd.unzip.sortOn fst.(++cached').zip ids $ vals

myPhi fs = fmap head $ myPhiMap (fs:[])

putInCache :: Features -> Double -> St ()
putInCache fs val = do
  root <- gets root
  srvr <- gets server
  liftIO $ C.set srvr (getKey root fs) (show val)
  return ()

lookupCache :: Features -> St (Maybe Double)
lookupCache fs
    | size fs == 0 = return (Just 0)
    | otherwise = do
  root <- gets root
  srvr <- gets server
  liftIO.fmap (fmap read) $ C.get srvr (getKey root fs)

getKey :: Int -> Features -> String
getKey root fs = show root ++ show (toNumber fs)

myFoundIrreducible :: Features -> Int -> St ()
myFoundIrreducible fs chosen = do
  fs' <- toNodeNames fs
  (c:_)  <-  toNodeNames (fromList [chosen])
  liftIO $ do
    putStrLn $ "Chose " ++ show c ++ " in " ++ show fs'
    putStrLn "Wating for key..."
    getChar
    putStrLn "Continuing"
