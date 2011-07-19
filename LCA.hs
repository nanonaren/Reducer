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
import Data.Maybe (fromJust,isNothing,isJust)
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
  useThis <- execStateT (setupCache >> setupR >>
                         setupNodes >> loadLongNames >>
                         setupFeatures) lca'
  when (bestfirst args) $ runBestFirst useThis
  case interactive args of
    False -> when (mine args) $
             evalStateT (summaryHeader >> getFeaturesForReduction >>=
                         runAll) useThis
    True -> when (mine args) $
            evalStateT (summaryHeader >> getFeaturesForReduction >>=
                        runInteractive) useThis

runBestFirst lca = undefined {-do
  stuff <- evalStateT (getFeatures >>= \fs ->
                       greedy fs myPhi 0 5 >>= \(fs',v) ->
                       toNodeNames fs' >>= \names ->
                       gets numCalls >>= \num ->
                       return (num,names,v)) lca
  print stuff-}

getFeaturesForReduction :: St Features
getFeaturesForReduction = do
  fs <- getFeatures
  reduceNNLS <- gets (reducennls.options)
  m <- gets (measure.options)
  case m of
    DistMeasure -> modify (\st -> st{options=(options st){maxFits=1}})
    _ -> return ()
  case reduceNNLS of
    False -> return fs
    True -> do
      (val,coeffs) <- myPhiWithCoeff fs
      let fs' = fromList.map fst.filter ((>0).snd).zip (toList fs) $ coeffs
          maxFits' = val
      modify (\st -> st{options=(options st){maxFits=maxFits'}})
      return fs'

runAll fs = do
  target <- gets (maxFits.options)
  numSamples <- gets (samples.options)
  nruns <- gets (runs.options)
  let run i = modify (\st -> st{doc = empty,numCalls=0}) >>
              liftIO newStdGen >>=
              runR fs myPhi numSamples myFoundIrreducible
                   (\_ _ -> return ([],[])) info target fs >>=
              summaryRun i.diff fs
  mapM_ run [1..nruns]

runInteractive fs = do
  target <- gets (maxFits.options)
  numSamples <- gets (samples.options)
  let run i = modify (\st -> st{doc = empty,numCalls=0}) >>
              liftIO newStdGen >>=
              runR fs myPhi numSamples myFoundIrreducible
                   chooser info target fs >>=
              summaryRun i.diff fs
  run 1

info = lift.putStr

chooser fs lvl = do
  names <- toNodeNames fs
  fs' <- toLongNames names
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  lift $ print d

  includes <- getInput "Enter node numbers to choose (space separated): " names
  excludes <- getInput "Enter node numbers to discard (space separated): " names
  return (includes,excludes)

getInput str names = do
  lift.putStr $ str
  ws <- fmap words (lift $ hGetLine stdin)
  lst <- lift $ mapM readMaybe ws
  let missing = filter (not.flip elem names.fromJust).filter isJust $ lst
      error = not.null.filter isNothing $ lst
  case (error,null missing) of
    (False,True) -> fmap toList (fromNodeNames.map fromJust $ lst)
    (False,False) -> liftIO (print.yellow.text $
                             "These nodes " ++ show missing ++
                             " do not exist. Try again.") >>
                     getInput str names
    (True,_) -> liftIO (print.yellow.text $ "Error reading some nodes. Try again.") >>
                getInput str names

readMaybe :: String -> IO (Maybe Int)
readMaybe str = do
    case reads str of
      [] -> print (yellow.text $ str ++ " not a valid number.") >> return Nothing
      [(val,_)] -> return (Just val)

summaryHeader :: St ()
summaryHeader = do
  fs <- getFeatures
  tree <- gets (read.rootNode.options) >>= toLongName
  kchildren <- getKnownChildren
  nruns <- gets (runs.options)

  (val,coeffs) <- myPhiWithCoeff fs
  nodes <- toNodeNames fs >>= toLongNames >>=
           return.filter ((>0).snd).flip zip coeffs

  liftIO.print $
        param "Tree" (text tree) <$$>
        param "Known Nodes" (listNodes kchildren) <$$>
        param "Raw NNLS" (listNodesWithCoeffs nodes) <$$>
        param "Raw NNLS val" (double val) <$$>
        param "Number of nodes used" (int $ size fs) <$$>
        param "Number of runs" (int nruns) <$$> text ""

getKnownChildren :: St [String]
getKnownChildren = do
  kc <- gets (knownChildren.options)
  toLongNames.sort $ map read (split "," kc)

summaryRun :: Int -> Features -> St ()
summaryRun i fs = do
  d <- gets doc
  calls <- gets numCalls
  (val,coeffs) <- myPhiWithCoeff fs
  nodes <- toNodeNames fs >>= toLongNames >>= return.flip zip coeffs
  liftIO.print $
        text "===== Run" <+> int i <+> text "=====" <$$>
        param "Num calls" (int calls) <$$>
        param "NNLS value" (double val) <$$>
        param "Discovered Tree" (listNodesWithCoeffs nodes) <$$>
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

listNodesWithCoeffs = vcat.map (\(n,v) -> text n <> colon <+> yellow (double v))
listNodes = vcat.map text

setupCache :: St ()
setupCache = do
  st <- get
  srvr <- liftIO $ connect "127.0.0.1" 11211
  put st{server = srvr}

setupNodes :: St ()
setupNodes = do
  st <- gets options
  mustInc <- fmap (map (head.words).lines) $ liftIO (readFile (mustInclude st))
  mustIncCat <- fmap (map (head.words).lines) $ liftIO (readFile (mustIncludeCat st))
  allnmsList <- fmap (flip zip [1..].map words.lines) $
                liftIO (readFile (namesFile st))
  let allnms = M.fromList.map (\(xs,i) -> (head xs,i)) $
               filter (flip elem mustInc.head.fst) allnmsList ++
               filter (flip elem mustIncCat.last.fst) allnmsList

  gen <- liftIO newStdGen
  let names = allnms
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

myPhiWithCoeff :: Features -> St (Double,[Double])
myPhiWithCoeff fs = do
  root <- gets root
  fromFS <- gets fromFeatures
  fmap head.nnlsMap root.(:[]).fromFS $ fs

myPhi fs = fmap head $ myPhiMap (fs:[])

putInCache :: Features -> Double -> St ()
putInCache fs val = do
  srvr <- gets server
  m <- gets (measure.options)
  key <- getKey fs
  liftIO $ C.set srvr key (show val)
  return ()

lookupCache :: Features -> St (Maybe Double)
lookupCache fs
    | size fs == 0 = return (Just 0)
    | otherwise = do
  srvr <- gets server
  key <- getKey fs
  liftIO.fmap (fmap read) $ C.get srvr key

getKey :: Features -> St String
getKey fs = do
  root <- gets root
  m <- gets (measure.options)
  let str = show root ++ show (toNumber fs)
  case m of
    DistMeasure -> return ('d' : str)
    FitMeasure -> return ('f' : str)

myFoundIrreducible :: Features -> [Int] -> Int -> St ()
myFoundIrreducible fs chosen lvl = do
  fs' <- toNodeNames fs >>= toLongNames
  chosen' <- toNodeNames (fromList chosen)
  lift.print.yellow $
      text "Choosing:" <+> (hsep.punctuate comma.map int $ chosen')
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "CHOSE:" <+> (hsep.punctuate comma.map int $ chosen')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  modify (\st -> st{doc = doc st <$$> d})
