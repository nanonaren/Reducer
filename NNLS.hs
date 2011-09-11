module NNLS
    (
      setupR
    , nnls
    ) where

import LCASt
import Control.Monad.State
import System.IO
import System.Process (runInteractiveCommand)

nnls :: Int -> [Int] -> St (Double,[Double])
nnls _ [] = return (0,[])
nnls root xs = do
  (inp,out) <- gets rp
  maxfits <- gets (maxFits.options)
  method <- gets (measure.options)
  delta <- gets (delta.options)
  let cmd = addCode method [1..198] xs root delta
  liftIO $ do
    hPutStr inp cmd
    ln <- skipStupidLines out
    let ws = words ln
        numFits = read.head $ ws
        coeffs = map read.tail $ ws
    return (numFits,coeffs)

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
  file <- gets (dataFile.options)
  nnlsPath <- gets (nnlsPath.options)
  rscriptPath <- gets (rscriptPath.options)
  (inp,out,_,_) <- liftIO $ runInteractiveCommand (rscriptPath ++ " --vanilla -")
  liftIO $ do
    hSetBuffering inp NoBuffering
    hSetBuffering out NoBuffering
    let scode = setupCode nnlsPath file
    hPutStr inp (setupCode nnlsPath file)
  modify (\s -> s{rp=(inp,out)})

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
