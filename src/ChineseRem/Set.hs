{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}
-- Uses independent membership distribution
module ChineseRem.Set
    (
      Info
    , SetFinder
    , create
    , run
    ) where

import ChineseRem
import SetUtils (randCoprimeFactors)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as S
import qualified Data.Map as M
import System.Random

type SetFinder u a b = StateT (Info u a b) (Writer [String])

data Info u a b = Info
    {
      iden :: S.Set a
    , rands :: [Bool]
    , cache :: M.Map (S.Set a) b
    , uSample :: u -> S.Set a -> S.Set a -> (u,b)
    , uIsomorph :: u -> (S.Set a,b) -> (S.Set a,b) -> (u,b)
    , userData :: u
    }

run m = fst.fst.runWriter.runStateT m

create uData uSam uIso elems = do
  gen <- newStdGen
  let inf = Info
            {
              iden = S.fromList elems
            , rands = randomRs (False,True) gen
            , cache = M.empty
            , uSample = uSam
            , uIsomorph = uIso
            , userData = uData
            }
  return inf

instance Ord a => Rem (S.Set a) b (SetFinder u a b) where
    coprimeFactors div = do
      rs <- gets rands
      e <- gets iden
      let (rs',res) = randCoprimeFactors rs e div
      modify (\s -> s{rands=rs'})
      return res

    sample a = do
      mval <- fetch a
      case mval of
        Just ps -> return ps
        Nothing ->
            do
              u <- gets userData
              f <- gets uSample
              idn <- gets iden
              let (u',ps) = f u idn a
              addToCache a ps
              modify (\s -> s{userData=u'})
              return ps
        where fetch x = gets (M.lookup x.cache)
              addToCache x ps = do
                 ch <- gets cache
                 modify (\s -> s{cache=M.insert x ps ch})

    isomorph ma mb = do
      a <- ma
      b <- mb
      u <- gets userData
      f <- gets uIsomorph
      let (u',res) = f u a b
      modify (\s -> s{userData=u'})
      return res
