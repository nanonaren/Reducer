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
import Data.IORef

main = do
  rootN <- fmap (read.(!!0)) getArgs
  hSetBuffering stdout NoBuffering
  fs76 <- get76Features
  featureMap <- getFeatures
  (Right cacheList) <- parseCSVFromFile "/home/narens/.cacher"
  let cache = M.fromList.map (\(x:y:_) -> (x,y)).filter ((==2).length) $ cacheList
      maxFits = 197
      root = rootN
      rootln = fromJust $ M.lookup root featureMap
      fs = S.delete root.S.fromList $ fs76

  cacheRef <- newIORef cache

  (inp,out) <- setupR
  let f = fmap fst.nnls cacheRef featureMap inp out maxFits rootln.S.toList
      rem = S.fromList [7101,10798,1154,1166,1072,1169,1056,664,1178,1174,9272,1983,1855,1817,1165,364,2115,1155]
  inf <- run (orchestra fs f rem)
--  f (S.fromList [271,1110,364,281,1844,6652,308,1171,7218,693,9272] )
--  f (S.fromList [1056,1169,7101,1154,1174,664,1171,1817,1855,1166,1072,1165,7083,7303,1983,411])
--  f (S.fromList [281,1171,1056,7224,9272] )
--  f (S.difference fs (S.fromList [1943,7303,9272]))
  return ()
--  putStrLn.show $ inf
--  sequenceCheck f inf

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

nnls :: (IORef (M.Map String String)) -> M.Map Int Int -> Handle -> Handle
     -> Double -> Int -> [Int] -> IO (Double,[Double])
nnls cacheRef mp inp out maxFits root xs = do
  cache <- readIORef cacheRef
  let xs' = map (fromJust.flip M.lookup mp) xs
      cmd = nnlscode [1..maxFits] xs' root
  ln <- case M.lookup (show root ++ ":" ++ show xs') cache of
    Nothing -> do
      hPutStr inp cmd
      let key = show root ++ ":" ++ show xs'
      value <- skipStupidLines out
      appendFile "/home/narens/.cacher" (printCSV [[key,value]]++ "\n")
      addToCache cacheRef key value
      return value
    Just v -> return v
  let ws = words ln
      numFits = read.head $ ws
      coeffs = map read.tail $ ws
--  putStrLn.show $ (numFits,coeffs)
  return (numFits,coeffs)

addToCache cacheRef key value = modifyIORef cacheRef (M.insert key value)

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