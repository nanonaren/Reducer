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
import ListUtils (rsortOn)
import Text.CSV
import System.Environment (getArgs)
import qualified Network.Memcache as C
import Network.Memcache.Protocol

main = do
  rootN <- fmap (read.(!!0)) getArgs
  hSetBuffering stdout NoBuffering
  fs76 <- get76Features
  featureMap <- getFeatures
  let maxFits = 197
      root = rootN
      rootln = fromJust $ M.lookup root featureMap
      fs = S.delete root.S.fromList $ fs76

  server <- connect "localhost" 11211

  (inp,out) <- setupR
  let f = fmap fst.nnls server featureMap inp out maxFits rootln.S.toList

  inf <- run (orchestra fs f 364)
--  mad <- f (S.difference fs (S.fromList [606,693,1072,1783,1852,271,308,382,1074,1088,1110,1155,1178,1181,1817,1844,2288,2291]))
--  putStrLn.show $ 1-mad
--  f (S.fromList [271,1110,364,281,1844,6652,308,1171,7218,693,9272] )
--  f (S.fromList [1056,1169,7101,1154,1174,664,1171,1817,1855,1166,1072,1165,7083,7303,1983,411])
--  val <- f (S.fromList [281,1171,1056,7224,9272] )
--  putStrLn.show $ val
--  f (S.difference fs (S.fromList [1943,7303,9272]))
  disconnect server
  return ()

sequenceCheck :: (S.Set Int -> IO Double) -> Info Int -> IO ()
sequenceCheck f inf = do
  mapM_ (\s -> f s >>= putStrLn.show) sets
    where fs = map fst.rsortOn snd.M.toList.ranking_ $ inf
          sets = map (S.fromList.flip take fs) [1..length fs]

getFeatures :: IO (M.Map Int Int)
getFeatures = do
  contents <- readFile "../data/allnames.tab"
  return.M.fromList.flip zip [1..].map (read.head.words).lines $ contents

get76Features :: IO [Int]
get76Features = do
  contents <- readFile "../data/76names.tab"
  return.map (read.head.words).lines $ contents

nnls :: Server -> M.Map Int Int -> Handle -> Handle
     -> Double -> Int -> [Int] -> IO (Double,[Double])
nnls server mp inp out maxFits root xs = do
  let xs' = map (fromJust.flip M.lookup mp) xs
      cmd = nnlscode [1..maxFits] xs' root
      key = show root ++ ":" ++ show xs'
  cacheVal <- C.get server key
  case cacheVal of
    Nothing -> do
      hPutStr inp cmd
      ln <- skipStupidLines out
      let ws = words ln
          fit = read.head $ ws
      C.set server key (show fit)
      return (fit,[])
    Just fit -> return (read fit,[])

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
--    "cat(cnt);\n" ++

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