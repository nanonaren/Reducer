{-# LANGUAGE DeriveDataTypeable #-}
module LCASt
    (
      LCA (..)
    , Options (..)
    , St
    , LCAMeasure (..)
    , lca
    , opts
    , incCallCount
    , getFeatures
    ) where

import Pipes
import Control.Monad.State
import qualified Data.Map as M
import Math.FeatureReduction.Features (Features,fromList)
import Network.Memcache.Protocol
import System.Console.CmdArgs
import System.IO
import Text.PrettyPrint.ANSI.Leijen

data LCAMeasure = FitMeasure | DistMeasure
    deriving (Show,Data,Typeable)

data Options = Options
    {
      maxFits :: Int
    , dataFile :: FilePath
    , nnlsPath :: FilePath
    , rscriptPath :: FilePath
    , rootNode :: String
    , delta :: Double
    , mustInclude :: FilePath
    , numRandomNodes :: Int
    , namesFile :: FilePath
    , measure :: LCAMeasure
    , knownChildren :: String
    , runs :: Int
    , samples :: Int
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
    , numCalls :: Int
    , longNames :: M.Map Int String
    , doc :: Doc
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
  , numCalls = 0
  , longNames = M.empty
  , doc = empty
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
  , measure = enum [FitMeasure &= help "Use fits to measure closeness",
                    DistMeasure &= help "Use distance to measure closeness"]
  , knownChildren = def &= help "Known children"
  , runs = 1 &= help "Number of runs" &= typ "INT"
  , samples = 20 &= help "Number of samples" &= typ "INT"
  }

incCallCount :: St ()
incCallCount = modify (\st -> st{numCalls = numCalls st + 1})

getFeatures :: St Features
getFeatures = gets (\st -> toFeatures st.M.keys.revNames $ st)