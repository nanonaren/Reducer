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
import qualified Data.Set as S
import qualified Data.Map as M
import System.Random

type SetFinder m a b = StateT (Info m a b) m

data Info m a b = Info
    {
      iden :: S.Set a
    , rands :: [Bool]
    , cache :: M.Map (S.Set a) b
    , uSample :: S.Set a -> S.Set a -> m b
    , uIsomorph :: (S.Set a,b) -> (S.Set a,b) -> m b
    }

run :: Monad m => SetFinder m a b r -> Info m a b -> m r
run m = evalStateT m

create uSam uIso elems = do
  gen <- newStdGen
  let inf = Info
            {
              iden = S.fromList elems
            , rands = randomRs (False,True) gen
            , cache = M.empty
            , uSample = uSam
            , uIsomorph = uIso
            }
  return inf

instance (Monad m,Ord a) => Rem (S.Set a) b (SetFinder m a b) where
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
              f <- gets uSample
              idn <- gets iden
              ps <- lift $ f idn a
              addToCache a ps
              return ps
        where fetch x = gets (M.lookup x.cache)
              addToCache x ps = do
                 ch <- gets cache
                 modify (\s -> s{cache=M.insert x ps ch})

    isomorph ma mb = do
      a <- ma
      b <- mb
      f <- gets uIsomorph
      res <- lift $ f a b
      return res
