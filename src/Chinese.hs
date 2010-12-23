{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,FunctionalDependencies #-}
module Chinese
    (
    ) where

import Control.Monad
import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (partition,(\\))
import System.Random
import ListUtils

class Monad m => Rem a b m | m -> a b where
    coprimeFactors :: a -> m (Maybe (a,a))
    sample :: a -> m b
    isomorph :: m b -> m b -> m b

data Info a b = Info
    {
      iden :: S.Set a
    , sampleTarget :: S.Set a
    , cache :: M.Map a b
    , rands :: [Bool]
    }

--main :: IO (M Int)
main = do
  st <- initSubset (S.fromList [1..10]) (S.fromList [1,3,5])
  let (a,w) = run st
  mapM_ putStrLn w
  return a

run st = runWriter.evalStateT (mad (S.fromList [])) $ st

mad :: S.Set Int -> Subset Int (M Int) (M Int)
mad a = improve a

initSubset :: S.Set Int -> S.Set Int -> IO (Info Int (M Int))
initSubset iden tar = do
  gen <- newStdGen
  let rs = randomRs (False,True) gen
  return $ Info iden tar M.empty rs

type Subset a b = StateT (Info a b) (Writer [String])
type M a = M.Map a Double

class Reserved a where
    reserved :: a

instance Reserved Int where
    reserved = 0

instance (Show a,Ord a,Reserved a) => Rem (S.Set a) (M a) (Subset a (M a)) where
    coprimeFactors d = do
      diff <- gets (flip S.difference d.iden)
      if S.size diff == 1
       then logg (show d ++ " is a prime") >> return Nothing
       else do
         (p1,p2) <- fmap (coprimes d diff) (takeRands (S.size diff))
         logg ("Factorizing " ++ show d ++ " into (" ++ show p1
               ++ ", " ++ show p2 ++ ")")
         return (Just (p1,p2))
        where coprimes com ext rands = (com`S.union`p1', com`S.union`p2')
                  where (p1,p2) = partition' fst (zip rands (S.toList ext))
                        p1' = S.fromList $ map snd p1
                        p2' = S.fromList $ map snd p2

    sample a = do
      target <- gets sampleTarget
      idn <- gets (S.toList.iden)
      let extras = S.toList $ target `S.difference` a
          zeroR = S.size (a `S.intersection` target) < S.size a
          elems = extras ++ if zeroR then [reserved] else []
          numElems = fromIntegral (length elems)
          probs = M.fromList $ zip elems (repeat (1/numElems))
      logg $ "Sampling " ++ show a ++ " to get " ++ show probs
      return probs

    isomorph ma mb = do
      da <- ma
      db <- mb
      let zeroA = getZero da
          zeroB = getZero db
          da' = M.map (zeroB*) da
          db' = M.map (zeroA*) db
          final = normalize $ M.insert reserved (zeroA*zeroB) (M.union da' db')
      logg $ "Isomorphing " ++ show da ++ " and " ++ show db ++ " to get " ++ show final
      return final

normalize m = M.map (/total) m
    where total = M.fold (+) 0 m

partition' f xs
    | null p1 = ([head p2],tail p2)
    | null p2 = (tail p1,[head p1])
    | otherwise = (p1,p2)
    where (p1,p2) = partition f xs

getZero m =
    case M.lookup reserved m of
      Nothing -> 0
      Just x -> x

takeRands n = do
  (xs,ys) <- gets (splitAt n.rands)
  modify (\s -> s{rands = ys})
  return xs
logg x = tell [x]

-- eta, depth -> eta
eta :: Double -> Int -> Double
eta p 0 = p
eta p d = 1 - (1-e)^2
    where e = eta p (d-1)

-- eta, required eta -> depth
requiredDepth :: Double -> Double -> Int
requiredDepth e req = head.dropWhile ((<req).eta e) $ [0..]

improve :: (Rem a b m) => a -> m b
improve a = do
  facs <- coprimeFactors a
  case facs of
    Nothing -> sample a
    Just (x,y) -> improve x `isomorph` improve y

tests =
    [
     testGroup "Error Tests"
       [
        testProperty "depth" prop_eta_depth_monotonic
       ,testProperty "prob" prop_eta_prob_monotonic
       ,testProperty "requiredDepth" prop_requiredDepth_monotonic
       ]
    ]

prop_eta_depth_monotonic =
    forAll (choose (0,1)) $ \p ->
    forAll (choose (0,10)) $ \d ->
    forAll (choose (0,10)) $ \d' ->
    let lhs = eta p d
        rhs = eta p d'
    in 
      d <= d' && lhs <= rhs || d > d' && lhs >= rhs

prop_eta_prob_monotonic =
    forAll (choose (0,1)) $ \p ->
    forAll (choose (0,1)) $ \p' ->
    forAll (choose (0,10)) $ \d ->
    let lhs = eta p d
        rhs = eta p' d
    in 
      p <= p' && lhs <= rhs || p > p' && lhs >= rhs

prop_requiredDepth_monotonic =
    forAll (choose (0,1)) $ \e ->
    forAll (choose (0,1)) $ \req ->
    forAll (choose (0,1)) $ \req' ->
    let lhs = requiredDepth e req
        rhs = requiredDepth e req'
    in
      req <= req' && lhs <= rhs || req > req' && lhs >= rhs

--prop: m=a*b , a <> m and b <> m
--prop: m=a*b , a U b = S, a intersect b = m
--prop: results should be elem permutation indifferent