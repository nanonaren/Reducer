module Main
    (
      main
    ) where

import DimensionLearner
import qualified Data.Set as S
import System.Process (runInteractiveCommand)
import System.IO
import Data.List ((\\))

main = do
  let maxFits = 197
      root = 60
      fs = S.fromList $ [1..76] \\ [root]
  (inp,out) <- setupR
  let f = fmap fst.nnls inp out maxFits root.S.toList
  inf <- run (orchestra 10 40 fs f)
  putStrLn.show $ inf

nnls :: Handle -> Handle -> Double -> Int -> [Int] -> IO (Double,[Double])
nnls inp out maxFits root xs = do
  let cmd = nnlscode [1..maxFits] xs root
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
  let file = "/home/narens/work/lca/data/allnodes.tab"
      nnlsPath = "/home/narens/Downloads/nnls"
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