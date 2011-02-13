module DimensionLearner
    (
    ) where

import SetUtils
import Control.Monad.Random
import Control.Monad.State (liftIO)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified DoubleMap as D
import System.Random

-- |Does stuff
orchestra :: Ord a =>
             Int -- ^Number of partitions
          -> Int -- ^Number of repetitions
          -> S.Set a -- ^The set of features
          -> (S.Set a -> IO Double) -- ^The objective function
          -> IO ()
orchestra np nr s f = do
  parts <- evalRandIO $ fmap (concat.take nr) (randPartitions np s)
  partws <- mapM f $ parts
  mapM_ (\(pw,p) -> subOrchestra s f pw p).zip partws $ parts

subOrchestra s f partw part = do
  cofacs <- evalRandIO $ randCoprimeFactors s part
  case cofacs of
    Nothing -> putStrLn.show $ "No factors to look at"
    Just (f1,f2) -> do
                f1w <- f f1
                f2w <- f f2
                putStrLn.show $ (partw - (f1w+f2w))