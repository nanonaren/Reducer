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

import Control.Monad.State
import qualified Data.Map as M
import Math.FeatureReduction.Features (Features,fromList,union)
import Network.Memcache.Protocol
import System.Console.CmdArgs
import System.IO
import Text.PrettyPrint.ANSI.Leijen

data LCAMeasure = FitMeasure | DistMeasure
    deriving (Show,Data,Typeable)

data Options = Options
    {
      maxFits :: Double
    , dataFile :: FilePath
    , nnlsPath :: FilePath
    , rscriptPath :: FilePath
    , rootNode :: String
    , delta :: Double
    , mustInclude :: FilePath
    , excludes :: FilePath
    , mustIncludeCat :: FilePath
    , numRandomNodes :: Int
    , namesFile :: FilePath
    , outputFile :: FilePath
    , measure :: LCAMeasure
    , knownChildren :: String
    , runs :: Int
    , samples :: Int
    , interactive :: Bool
    , reducennls :: Bool
    , usememcache :: Bool
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
    , numCalls :: Int
    , longNames :: M.Map Int String
    , doc :: Doc
    , outHandle :: Handle
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
  , numCalls = 0
  , longNames = M.empty
  , doc = empty
  , outHandle = undefined
  }

type St = StateT LCA IO

opts = Options
  {
    maxFits = 198 &= help "Number of impact factors"
  , dataFile = "data/allnodes.tab" &= help "Data file" &= typFile
  , nnlsPath = "data/nnls" &= help "NNLS library path" &= typDir
  , rscriptPath = "/usr/bin/Rscript" &= help "Rscript path" &= typFile
  , rootNode = def &= help "Root node" &= typ "INT"
  , delta = 0.15 &= help "Allowable fit error" &= typ "DOUBLE"
  , mustInclude = "data/includenodes" &= help "Must include nodes" &= typFile
  , excludes = "data/excludenodes" &= help "Must exclude nodes" &= typFile
  , mustIncludeCat = "data/includecat" &= help "Must include categories" &= typFile
  , numRandomNodes = 0 &= help "Number of extra random nodes to include" &= typ "INT"
  , namesFile = "data/allnames.tab" &= help "Node names file" &= typFile
  , outputFile = "output" &= help "File to output results to" &= typFile
  , measure = enum [FitMeasure &= help "Use fits to measure closeness",
                    DistMeasure &= help "Use distance to measure closeness"]
  , knownChildren = def &= help "Known children"
  , runs = 1 &= help "Number of runs" &= typ "INT"
  , samples = 5 &= help "Number of samples" &= typ "INT"
  , interactive = False &= help "Enable to choose element from irreducible"
  , reducennls = False &= help "Just reduce the positive nodes that nnls outputs"
  , usememcache = False &= help "Use memcache"
  } &= program "LCA Reducer"
    &= summary "Reduce number of nodes required to get a fit"
    &= details ["http://github.com/nanonaren/Reducer"]

incCallCount :: St ()
incCallCount = modify (\st -> st{numCalls = numCalls st + 1})

getFeatures :: St Features
getFeatures = gets (\st -> toFeatures st.M.keys.revNames $ st)
