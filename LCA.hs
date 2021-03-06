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
import NanoUtils.Tuple (swap,mapHomTup)

import Control.Monad.State
import Data.Maybe (fromJust,isNothing,isJust)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.CmdArgs
import System.IO
import System.Random hiding (split)

import qualified Network.Memcache as C
import Network.Memcache.Protocol
import Text.PrettyPrint.ANSI.Leijen

import Numeric (showFFloat)

main = do
  hSetBuffering stdout NoBuffering
  args <- cmdArgs opts
  out <- openFile (outputFile args ++ ".run") WriteMode
  hSetBuffering out NoBuffering
  let lca' = lca{options = args,outHandle = out}
  useThis <- execStateT (setupCache args >> setupR >>
                         setupNodes >> loadLongNames >>
                         setupFeatures) lca'
  case interactive args of
    False -> evalStateT (summaryHeader >> getFeaturesForReduction >>=
                         runAll) useThis
    True -> evalStateT (summaryHeader >> getFeaturesForReduction >>=
                        runInteractive) useThis

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
                   chooserEcho info target fs >>=
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

-- | print both to output file and stdout
info str = do
  h <- gets outHandle
  lift.hPutStr h $ str
  lift.putStr $ str

printInfo = info.show

printCoeffsToFile doc = do
  file <- gets ((++".coeffs").outputFile.options)
  lift $ writeFile file (show doc)

chooserEcho fs lvl = do
  names <- toNodeNames fs
  fs' <- toLongNames names
  let d = text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs') <$> empty
  printInfo d
  return ([],[])

chooser fs lvl = do
  names <- toNodeNames fs
  fs' <- toLongNames names
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  printInfo (d <$> empty)

  includes <- getInput "Enter node numbers to choose (space separated): " names
  excludes <- getInput ("Enter node numbers to discard (space separated; 0 to discard all\n" ++
                         "                                              ; 1 to quit\n" ++
                         "                                              ; 2 to switch to automatic): ") (0:1:2:names)
  case elem 0 excludes of
    True -> return ([],toList fs)
    False -> case elem 1 excludes of
               True -> return ([1],[])
               False -> case elem 2 excludes of
                          True -> return ([2],[])
                          False -> return (includes,excludes)

getInput str names = do
  lift.putStr $ str
  ws <- fmap words (lift $ hGetLine stdin)
  lst <- lift $ mapM readMaybe ws
  let missing = filter (not.flip elem names.fromJust).filter isJust $ lst
      error = not.null.filter isNothing $ lst
  case (error,null missing) of
    (False,True) -> if elem (Just 0) lst then return [0]
                    else if elem (Just 1) lst then return [1]
                    else if elem (Just 2) lst then return [2]
                    else fmap toList (fromNodeNames.map fromJust $ lst)
    (False,False) -> liftIO (print.text $
                             "These nodes " ++ show missing ++
                             " do not exist. Try again.") >>
                     getInput str names
    (True,_) -> liftIO (print.text $ "Error reading some nodes. Try again.") >>
                getInput str names

readMaybe :: String -> IO (Maybe Int)
readMaybe str = do
    case reads str of
      [] -> print (text $ str ++ " not a valid number.") >> return Nothing
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

  printInfo $
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
  let coeffsDoc = listNodesWithCoeffs nodes
  printCoeffsToFile coeffsDoc

  printInfo $
        text "===== Run" <+> int i <+> text "=====" <$$>
        param "Num calls" (int calls) <$$>
        param "NNLS value" (double val) <$$>
        param "Discovered Tree" coeffsDoc <$$>
        param "Irreducibles" d <$$> text ""
  printImpactStatsToFile fs

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

listNodesWithCoeffs = vcat.map (\(n,v) -> text n <> colon <+> double v)
listNodes = vcat.map text

setupCache :: Options -> St ()
setupCache args = when (usememcache args) $ do
  st <- get
  srvr <- liftIO $ connect "127.0.0.1" 11211
  put st{server = srvr}

setupNodes :: St ()
setupNodes = do
  st <- gets options
  mustInc <- fmap (map (head.words).lines) $ liftIO (readFile (mustInclude st))
  mustExclude <- fmap (map (head.words).lines) $ liftIO (readFile (excludes st))
  mustIncCat <- fmap (map (head.words).lines) $ liftIO (readFile (mustIncludeCat st))
  allnmsList <- fmap (flip zip [1..].map words.lines) $
                liftIO (readFile (namesFile st))
  let allnms = M.fromList.map (\(xs,i) -> (head xs,i)).
               filter (not.flip elem mustExclude.head.fst) $
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

myPhi :: Features -> St Double
myPhi fs = do
  root <- gets root
  fromFS <- gets fromFeatures
  maybeVal <- lookupCache fs
  incCallCount
  case maybeVal of
    Nothing -> do
             (val,_) <- nnls root.fromFS $ fs
             putInCache fs val
             return val
    Just val -> return val

myPhiWithCoeff :: Features -> St (Double,[Double])
myPhiWithCoeff fs = do
  root <- gets root
  fromFS <- gets fromFeatures
  nnls root.fromFS $ fs

putInCache :: Features -> Double -> St ()
putInCache fs val = do
  usecache <- gets (usememcache.options)
  when usecache $ do
    srvr <- gets server
    m <- gets (measure.options)
    key <- getKey fs
    liftIO $ C.set srvr key (show val)
    return ()

lookupCache :: Features -> St (Maybe Double)
lookupCache fs = do
  usecache <- gets (usememcache.options)
  if usecache then lookupCache' fs else return Nothing
lookupCache' fs
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
  printInfo $
      text "Choosing:" <+> (hsep.punctuate comma.map int $ chosen') <$> empty
  let d = hsep.punctuate semi $
          [ text "LEVEL:" <+> (int lvl)
          , text "ACTUAL:" <+> (int $ length fs')
          , text "CHOSE:" <+> (hsep.punctuate comma.map int $ chosen')
          , text "IRRED:" <+> (braces.align.vsep.punctuate comma.map text $ fs')
          ]
  modify (\st -> st{doc = doc st <$$> d})

printImpactStatsToFile :: Features -> St ()
printImpactStatsToFile fs = do
  file <- gets ((++".factors").outputFile.options)
  h <- lift $ openFile file WriteMode
  lift.putStr $ "Writing impact factor stats..."
  probeSolution fs h
  lift.putStr $ "DONE\n"
  lift $ hClose h

probeSolution :: Features -> Handle -> St ()
probeSolution fs outH = do
  root <- gets root
  (_,coeffs) <- myPhiWithCoeff fs
  nodeLineNums <- gets ((root:).($fs).fromFeatures)
  file <- gets (dataFile.options)
  rows <- fmap (filter (flip elem nodeLineNums.fst).
                zip [1..].map words.lines).
               lift.readFile $ file

  let ((rootRow:_),childRows) = mapHomTup (map snd).
                                partition ((==root).fst) $ rows
      childData = zipWith (\c v -> map (c*) v) coeffs.
                  map (map read) $ childRows
      parentData = map read rootRow

  mapM_ (percentageContributions (parentData : childData) coeffs fs outH) [0..197]

-- print percentage contributions of nodes for a particular impact factor
percentageContributions :: [[Double]] -> [Double] -> Features -> Handle -> Int -> St ()
percentageContributions dataPoints coeffs fs outH factor = do
  nodes <- toNodeNames fs >>= toLongNames
  let (rootv:restv) = map (!!factor) dataPoints
      linCombVal = sum restv
      contribs = map ((/linCombVal).(*100)) restv

  lift.hPutStrLn outH.show $ param "Impact factor number" (text "#" <> int factor)
    <$$> param "Actual parent value" (double rootv)
    <$$> param "Linearly combined value" (double linCombVal)
    <$$> param "Percentage error" (convert.abs $ (linCombVal-rootv)*100 / rootv)
    <$$> listContribWithCoeffs (rsortOn (\(_,c,_) -> c) (zip3 nodes contribs coeffs))
    <$> empty
    where listContribWithCoeffs = vcat.(fill 15 (text "Contribution") <+> fill 15 (text "Coefficient") <+> text "Node":).
                                  map (\(n,v,c) -> fill 15 (convert $ v) <+>
                                                   fill 15 (convert $ c) <+> 
                                                   text n)
          convert = text.($"").showFFloat (Just 4)

-- from package missingH, just want to avoid dependency
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)