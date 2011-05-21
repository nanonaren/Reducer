{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TupleSections #-}
module Main
    (
      main
    ) where

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
import System.Process (runInteractiveCommand)
import System.Random
import System.Random.Shuffle (shuffle')

import qualified Network.Memcache as C
import Network.Memcache.Protocol
import Pipes

data Options = Options
    {
      maxFits :: Int,
      dataFile :: FilePath,
      nnlsPath :: FilePath,
      rscriptPath :: FilePath,
      rootNode :: String,
      delta :: Double,
      mustInclude :: FilePath,
      numRandomNodes :: Int,
      namesFile :: FilePath
    } deriving (Show,Data,Typeable)

data LCA = LCA
    {
      options :: Options
    , rp :: (Handle,Handle)
    , names :: M.Map String Int
    , revNames :: M.Map Int String
    , toFeatures :: [Int] -> Features
    , fromFeatures :: Features -> [Int]
    , root :: Int
    , server :: Server
    , pipes :: Pipes
    }

lca = LCA
  {
    options = undefined
  , rp = (stdin,stdout)
  , names = M.empty
  , revNames = M.empty
  , toFeatures = undefined
  , fromFeatures = undefined
  , root = 0
  , server = undefined
  , pipes = undefined
  }

type St = StateT LCA IO

opts = Options
  {
    maxFits = 198 &= help "Number of impact factors"
  , dataFile = "/home/narens/work/chinese/data/allnodes.tab" &= help "Data file" &= typFile
  , nnlsPath = "/home/narens/Downloads/nnls" &= help "NNLS library path" &= typDir
  , rscriptPath = "/usr/bin/Rscript" &= help "Rscript path" &= typFile
  , rootNode = def &= help "Root node" &= typ "INT"
  , delta = 0.15 &= help "Allowable fit error" &= typ "DOUBLE"
  , mustInclude = "/home/narens/work/lca/data/76names.tab" &= help "Must include nodes" &= typFile
  , numRandomNodes = 0 &= help "Number of extra random nodes to include" &= typ "INT"
  , namesFile = "/home/narens/work/lca/data/allnames.tab" &= help "Node names file" &= typFile
  }

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

nnlsMap :: Int -> [[Int]] -> St [(Double,[Double])]
nnlsMap root xss = do
  ps <- gets pipes
  maxfits <- gets (maxFits.options)
  let cmds = map (\xs -> nnlscode [1..maxfits] xs root) xss
  liftIO $ pushMap ps handler cmds
    where handler h = skipStupidLines h >>= \ln ->
                      let ws = words ln
                          numFits = read.head $ ws
                          coeffs = map read.tail $ ws
                      in return (numFits,coeffs)

-- nnls :: Int -> [Int] -> St (Double,[Double])
-- nnls root xs = do
--   (inp,out) <- gets rp
--   maxfits <- gets (maxFits.options)
--   let cmd = nnlscode [1..maxfits] xs root
--   liftIO $ hPutStr inp cmd
--   ln <- liftIO (skipStupidLines out)
--   let ws = words ln
--       numFits = read.head $ ws
--       coeffs = map read.tail $ ws
--   return (numFits,coeffs)

nnlscode factors children root =
    "parent <- t(m[" ++ show root ++ ",])[c(" ++ toRarr factors ++ "),]\n" ++
    "children <- t(m[c(" ++ toRarr children ++ "),])[c(" ++
                         toRarr factors ++ "),]\n" ++
    "n <- nnls(children, parent)\n" ++
    "rv = 0;clen = length(children[1,]);\n" ++
    "for (i in 1:clen){rv = rv + n$x[i] * t(children)[i,]}\n" ++ 
    "di = 0\n" ++
    "for (i in 1:length(parent)){di = di + (parent[i]-rv[i])^2}\n" ++
    "dip=0\n" ++
    "for (i in 1:length(parent)){dip = dip + (parent[i])^2}\n" ++
    "val = (dip-di)/dip; cat(val);\n" ++

    "for(i in 1:length(n$x))\n" ++
    "{\n" ++
    "cat(' ');\n" ++
    "cat(n$x[i]);\n" ++
    "}\n" ++
    "cat('\\n')\n"

setupR :: St ()
setupR = do
  ps <- liftIO initPipes
  file <- gets (dataFile.options)
  nnlsPath <- gets (nnlsPath.options)
  rscriptPath <- gets (rscriptPath.options)
  liftIO $ do
--    addShellPipe ps (rscriptPath ++ " --vanilla -")
    addShellPipe ps ("ssh `cat ~/ip` '" ++ rscriptPath ++ " --vanilla -" ++ "'")
    let scode = setupCode nnlsPath file
    pushMap ps (\_ -> return ()) [scode]
  modify (\s -> s{pipes=ps})

setupCode libloc loc =
    (if null libloc then "library(nnls)\n"
     else "library(nnls,lib.loc=" ++ show libloc ++ ")\n") ++
    "m<-read.table(" ++ show loc ++ ",header=FALSE);\n"

skipStupidLines h = do
  ln <- hGetLine h
  if null ln || head ln == ' ' then skipStupidLines h else return ln

toRarr :: Num a => [a] -> String
toRarr (x:[]) = show x ++ "," ++ show x
toRarr xs = init.concat.map ((++",").show) $ xs