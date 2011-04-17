{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TupleSections #-}
module Main
    (
      main
    ) where

import ChineseRem (divisorSearch2)
import ChineseRem.Set (run)
import ChineseRem.IndepSet
import ListUtils (rsortOn)

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
    , root :: Int
    }

lca = LCA
  {
    options = opts
  , rp = (stdin,stdout)
  , names = M.empty
  , revNames = M.empty
  , root = 0
  }

type St = StateT LCA IO

opts = Options
  {
    maxFits = 197
  , dataFile = "/home/narens/work/lca/data/allnodes.tab"
  , nnlsPath = "/home/narens/Downloads/nnls"
  , rscriptPath = "/usr/bin/Rscript"
  , rootNode = "10158"
  , delta = 0.15
  , mustInclude = "/home/narens/work/lca/data/76names.tab"
  , numRandomNodes = 0
  , namesFile = "/home/narens/work/lca/data/allnames.tab"
  }

instance Reserved Int where
    reserved = 0

main = do
  st <- execStateT (setupR >> setupNodes) lca
  finder <- create sampler 1 (M.elems.M.delete (rootNode opts).names $ st)
  res <- flip evalStateT st.run (doIt 1) $ finder
  putStrLn.show.rsortOn snd.M.toList $ res

doIt n = do
  dss <- sequence (replicate n $ divisorSearch2 (S.fromList []))
  return.foldl' (M.unionWith (+)) M.empty $ dss

sampler :: S.Set Int -> S.Set Int -> St (MDist Int)
sampler iden a = do
  liftIO.putStrLn.show $ "HELLO, I am in the sampler"
  root <- gets root
  mfits <- gets (fromIntegral.maxFits.options)
  (fits,cs) <- nnls root (S.toList a)
  let rem = S.difference iden a
      remList = S.toList rem
      p = fromIntegral fits/mfits
      shared = (1-p) / fromIntegral (S.size rem)
      ashared = p / fromIntegral (S.size a)
      dist = M.fromList $ map (,ashared) (S.toList a) ++ map (,shared) remList
--      dist = M.fromList $ fromCoeffs a p cs ++ map (,shared) remList
  liftIO.putStrLn.show $ "Remainder " ++ show remList ++ " got "
                         ++ show shared ++ " FITS: " ++ show fits
  --liftIO.putStrLn.show $ dist
  return dist

fromCoeffs a p = zip (S.toList a).map (*p).normalize

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
      revNames = M.fromList.map (\(a,b) -> (b,a)).M.toList $ names
      rootNum = fromJust.M.lookup (rootNode st) $ names
  modify (\s -> s{names=names,revNames=revNames,root=rootNum})


nnls :: Int -> [Int] -> St (Int,[Double])
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
    "cnt=0;\n" ++
    "for (i in 1:length(parent))\n" ++
    "{\n" ++
    "d <- abs(n$residuals[i] / parent[i])\n" ++
    "if (d < 0.15)\n" ++
    "{\n" ++ 
    "cnt=cnt+1;\n" ++
    "}\n}\n" ++
    "cat(cnt);\n" ++
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