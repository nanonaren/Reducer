module Main
    (
      main
    ) where

import DimensionLearner
import qualified Data.Set as S
import System.Process (runInteractiveCommand)
import System.IO
import Data.Maybe (fromJust)
import qualified Data.Map as M

main = do
  fs76 <- get76Features
  featureMap <- getFeatures
  let maxFits = 197
      root = 7063
      rootln = fromJust $ M.lookup root featureMap
      fs = S.delete root.S.fromList $ fs76
  (inp,out) <- setupR
  let f = fmap fst.nnls featureMap inp out maxFits rootln.S.toList
  inf <- run (orchestra 75 1 fs f)
  putStrLn.show $ inf

getFeatures :: IO (M.Map Int Int)
getFeatures = do
  contents <- readFile "../data/allnames.tab"
  return.M.fromList.flip zip [1..].map (read.head.words).lines $ contents

get76Features :: IO [Int]
get76Features = do
  contents <- readFile "../data/76names.tab"
  return.map (read.head.words).lines $ contents

nnls :: M.Map Int Int -> Handle -> Handle -> Double
     -> Int -> [Int] -> IO (Double,[Double])
nnls mp inp out maxFits root xs = do
  let xs' = map (fromJust.flip M.lookup mp) xs
      cmd = nnlscode [1..maxFits] xs' root
  hPutStr inp cmd
  ln <- skipStupidLines out
  let ws = words ln
      numFits = read.head $ ws
      coeffs = map read.tail $ ws
--  putStrLn.show $ (numFits/maxFits,coeffs)
  return (numFits/maxFits,coeffs)

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

setupR :: IO (Handle,Handle)
setupR = do
  let file = "../data/allnodes.tab"
      nnlsPath = "../data/nnls"
      rscriptPath = "/usr/bin/Rscript"
  (inp,out,_,_) <- runInteractiveCommand (rscriptPath ++ " --vanilla -")
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hPutStr inp (setupCode nnlsPath file)
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