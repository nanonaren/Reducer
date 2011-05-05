{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TupleSections #-}
module Main
    (
      main
    ) where

import Math.FeatureReduction.Features
import NanoUtils.List (rsortOn)
import NanoUtils.Tuple (swap)

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.CmdArgs
import System.IO
import System.Process (runInteractiveCommand)
import System.Random
import System.Random.Shuffle (shuffle')

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
    }

lca = LCA
  {
    options = opts
  , rp = (stdin,stdout)
  , names = M.empty
  , revNames = M.empty
  , toFeatures = undefined
  , fromFeatures = undefined
  , root = 0
  }

type St = StateT LCA IO

opts = Options
  {
    maxFits = 198
  , dataFile = "/home/narens/work/chinese/data/allnodes.tab"
  , nnlsPath = "/home/narens/Downloads/nnls"
  , rscriptPath = "/usr/bin/Rscript"
  , rootNode = "7063"
  , delta = 0.15
  , mustInclude = "/home/narens/work/lca/data/76names.tab"
  , numRandomNodes = 0
  , namesFile = "/home/narens/work/lca/data/allnames.tab"
  }

main = do
  val <- evalStateT (setupR >> setupNodes >> setupFeatures >> phi (fromList [1..70])) lca
  print val

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

phi :: Features -> St Double
phi fs = do
  xs <- gets (($fs).fromFeatures)
  liftIO (print xs)
  root <- gets root
  (v,_) <- nnls root xs
  return v

nnls :: Int -> [Int] -> St (Double,[Double])
nnls root xs = do
  (inp,out) <- gets rp
  maxfits <- gets (maxFits.options)
  let cmd = nnlscode [1..maxfits] xs root
  liftIO $ hPutStr inp cmd
  ln <- liftIO (skipStupidLines out)
  let ws = words ln
      numFits = read.head $ ws
      coeffs = map read.tail $ ws
  return (numFits,coeffs)

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
  rp <- setupR'
  modify (\s -> s{rp=rp})

setupR' :: St (Handle,Handle)
setupR' = do
  file <- gets (dataFile.options)
  nnlsPath <- gets (nnlsPath.options)
  rscriptPath <- gets (rscriptPath.options)
  (inp,out,_,_) <- liftIO $ runInteractiveCommand (rscriptPath ++ " --vanilla -")
  liftIO $ hSetBuffering inp NoBuffering
  liftIO $ hSetBuffering out NoBuffering
  liftIO $ hPutStr inp (setupCode nnlsPath file)
  return (inp,out)

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