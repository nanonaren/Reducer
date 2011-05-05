module Main
    (
      main
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Attoparsec.Char8 as P
import Data.Attoparsec.Lazy hiding (skipWhile,take)
import Data.List (intercalate,transpose)
import NanoUtils.Container (normalizeByMax)
import System.IO

main = do
  xss <- parseFile
  let xss' = transpose.map normalizeByMax.transpose $ xss
      contents = intercalate "\n".map (intercalate "\t".map show) $ xss'
  writeFile "temp" contents

parseFile = do
  contents <- L.readFile "data/allnodes_notnormalized.tab"
  let (Done _ lst) = parse docParser contents
  return lst

docParser = (P.double `P.sepBy` skipTab) `P.sepBy` P.endOfLine
skipTab = P.skipWhile (=='\t')