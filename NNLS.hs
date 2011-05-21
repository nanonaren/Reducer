module NNLS
    (
      setupR
    , nnlsMap
    ) where

import LCASt
import Pipes
import Control.Monad.State
import System.IO (hGetLine)

nnlsMap :: Int -> [[Int]] -> St [(Double,[Double])]
nnlsMap root xss = do
  ps <- gets pipes
  maxfits <- gets (maxFits.options)
  method <- gets (measure.options)
  delta <- gets (delta.options)
  let cmds = map (\xs -> addCode method [1..maxfits] xs root delta) xss
  liftIO $ pushMap ps handler cmds
    where handler h = skipStupidLines h >>= \ln ->
                      let ws = words ln
                          numFits = read.head $ ws
                          coeffs = map read.tail $ ws
                      in return (numFits,coeffs)

addCode FitMeasure factors children root delta =
    nnlsFitsCode factors children root delta
addCode DistMeasure factors children root _ =
    nnlsDistCode factors children root

nnlsDistCode factors children root =
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

nnlsFitsCode factors children root delta =
    "parent <- t(m[" ++ show root ++ ",])[c(" ++ toRarr factors ++ "),]\n" ++
    "children <- t(m[c(" ++ toRarr children ++ "),])[c(" ++
                         toRarr factors ++ "),]\n" ++
    "n <- nnls(children, parent)\n" ++
    "cnt=0;\n" ++
    "for (i in 1:length(parent))\n" ++
    "{\n" ++
    "d <- abs(n$residuals[i] / parent[i])\n" ++
    "if (d < " ++ show delta ++ ")\n" ++
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
  ps <- liftIO initPipes
  file <- gets (dataFile.options)
  nnlsPath <- gets (nnlsPath.options)
  rscriptPath <- gets (rscriptPath.options)
  liftIO $ do
    addShellPipe ps (rscriptPath ++ " --vanilla -")
--    addShellPipe ps ("ssh `cat ~/ip` '" ++ rscriptPath ++ " --vanilla -" ++ "'")
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
