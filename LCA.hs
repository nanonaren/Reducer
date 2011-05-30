{-# LANGUAGE NoMonomorphismRestriction,TupleSections #-}
module Main
    (
      main
    ) where

import LCASt
import NNLS
import Math.FeatureReduction.Stochastic
import Math.FeatureReduction.Features hiding (split)
import NanoUtils.List (rsortOn,sortOn)
import NanoUtils.Tuple (swap)

import Control.Monad.State
import Data.Maybe (fromJust,isNothing)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.CmdArgs
import System.IO
import System.Random hiding (split)
import System.Random.Shuffle (shuffle')

import qualified Network.Memcache as C
import Network.Memcache.Protocol
import Pipes
import Text.PrettyPrint.ANSI.Leijen
import Data.String.Utils (split)

import Math.FeatureReduction.Greedy

main = do
  hSetBuffering stdout NoBuffering
  args <- cmdArgs opts
  let lca' = lca{options = args}
  useThis <- execStateT (setupCache >> setupR >> setupNodes >> loadLongNames >>
                         setupFeatures) lca'
--  stuff <- evalStateT (getFeatures >>= \fs -> greedy fs myPhi 0 5 >>= \(fs',v) ->
--                       toNodeNames fs' >>= \names -> gets numCalls >>= \num -> return (num,names,v)) useThis
--  print stuff
  case interactive args of
    False -> evalStateT (summaryHeader >> runAll) useThis
    True -> evalStateT (runInteractive) useThis

runAll = do
  fs <- getFeatures
  target <- gets (maxFits.options)
  numSamples <- gets (samples.options)
  nruns <- gets (runs.options)
  let run i = modify (\st -> st{doc = empty,numCalls=0}) >>
              liftIO newStdGen >>=
              runR fs myPhi numSamples myFoundIrreducible (\_ _ -> return Nothing) (return False) info addToOuts 198 fs >>=
              summaryRun i.diff fs
  mapM_ run [1..nruns]

runInteractive = do
  fs <- getFeatures
  target <- gets (maxFits.options)
  numSamples <- gets (samples.options)
  let run i = modify (\st -> st{doc = empty,numCalls=0}) >>
              liftIO newStdGen >>=
              runR fs myPhi numSamples myFoundIrreducible chooser (return False) info addToOuts target fs >>=
              summaryRun i.diff fs
  run 1

info = lift.putStrLn

chooser fs lvl = do
  names <- toNodeNames fs
  fs' <- toLongNames names
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  lift $ print d
  lift.putStr $ "Enter node number to choose: "
  val <- fmap read (lift $ hGetLine stdin)
  case elem val names of
    True -> fromNodeNames [val] >>= return.(\x -> Just x).head.toList
    False -> lift (putStrLn "You choose an invalid element. Auto choosing.") >> return Nothing

clearRemaining = do
  lift $ putStr "Removing rest of nodes in irreducible? [y/n]"
  ln <- lift $ hGetLine stdin
  case ln of
    "y" -> return True
    _   -> return False

summaryHeader :: St ()
summaryHeader = do
  fs <- fmap toList getFeatures
  tree <- gets (read.rootNode.options) >>= toLongName
  kchildren <- getKnownChildren
  nruns <- gets (runs.options)

  rootN <- gets root
  ((val,coeffs):_) <- nnlsMap rootN [fs]
  fromNNLS <- (>>= toLongNames).toNodeNames.fromList.map fst.
              filter ((>0).snd).zip fs $ coeffs

  liftIO.print $
        param "Tree" (text tree) <$$>
        param "Known Nodes" (listNodes kchildren) <$$>
        param "Raw NNLS" (listNodes fromNNLS) <$$>
        param "Raw NNLS val" (double val) <$$>
        param "Number of nodes used" (int $ length fs) <$$>
        param "Number of runs" (int nruns) <$$> text ""

getKnownChildren :: St [String]
getKnownChildren = do
  kc <- gets (knownChildren.options)
  toLongNames.sort $ map read (split "," kc)

summaryRun :: Int -> Features -> St ()
summaryRun i fs = do
  d <- gets doc
  calls <- gets numCalls
  remThese <- gets outs
  nodes <- toNodeNames (diff fs remThese) >>= toLongNames
  liftIO.print $
        text "===== Run" <+> int i <+> text "=====" <$$>
        param "Num calls" (int calls) <$$>
        param "Discovered Tree" (listNodes nodes) <$$>
        param "Irreducibles" d <$$> text ""

toLongNames :: [Int] -> St [String]
toLongNames xs = do
  mp <- gets longNames
  return.map (\x -> ((show x ++ " ") ++).rep.fromJust.flip M.lookup mp $ x) $ xs
    where rep = map (\c -> if c == '_' then ' ' else c)
toLongName = fmap head.toLongNames.(:[])

loadLongNames :: St ()
loadLongNames = do
  names <- gets (namesFile.options)
  contents <- liftIO $ readFile names
  let mp = M.fromList.map (\l -> let (a:b:_) = words l in (read a,b)).lines $ contents
  modify (\st -> st{longNames = mp})

param name info = fill 30 (text name) <> colon <+> align info

listNodes = vcat.map text

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
  sequence_.replicate (length fss) $ incCallCount
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

myFoundIrreducible :: Features -> Int -> Int -> St ()
myFoundIrreducible fs chosen lvl = do
  fs' <- toNodeNames fs >>= toLongNames
  chosen' <- fmap head $ toNodeNames (fromList [chosen])
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "CHOSE:" <+> (int chosen')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  modify (\st -> st{doc = doc st <$$> d})
