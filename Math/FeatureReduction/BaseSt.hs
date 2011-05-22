module Math.FeatureReduction.BaseSt
    (
      FeatureInfo (..)
    , St
    , Value
    , Phi
    , Psi
    , addToWorkingSet
    , complement
    ) where

import Control.Monad.State
import qualified Data.Map as M
import Math.FeatureReduction.Features

data FeatureInfo m = FeatureInfo
    {
      allFS :: Features
    , lvl1and2 :: Features
    , workingSet :: Features
    , pickedAtLvl :: M.Map Int [Int]
    , phi :: Features -> m Value
    , psi :: Features -> Features -> m Value
    , foundIrreducible :: Features -> Int -> m ()
    , info :: String -> m ()
    }

type St m = StateT (FeatureInfo m) m
type Value = Double
type Phi m = Features -> m Value
type Psi m = Features -> Features -> m Value

addToWorkingSet :: Monad m => Int -> Features -> St m ()
addToWorkingSet lvl fs = do
  ws <- gets workingSet
  temp <- gets lvl1and2
  modify (\st -> st{workingSet = union ws fs})
  when (lvl < 2) $ modify (\st -> st{lvl1and2 = union temp fs})

-- | set complement of X in F
complement :: Monad m => Features -> St m Features
complement ireds = gets allFS >>= return.flip diff ireds