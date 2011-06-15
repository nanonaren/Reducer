module Math.FeatureReduction.Greedy
    (
      greedy
    ) where

import Data.List (maximumBy,delete,(\\))
import Data.Ord
import Math.FeatureReduction.Features hiding (delete)

greedy :: Monad m => (String -> m ()) -> Features -> (Features -> m Double) ->
          Double -> Double -> Int -> m (Features,Double)
greedy info fs f epsilon stopAt k = do
  let open = [(fromList [],0)]
  greedy' info fs f open [] (head open) epsilon stopAt k 0

greedy' :: Monad m => (String -> m ()) -> Features ->
           (Features -> m Double) -> [(Features,Double)] ->
           [(Features,Double)] -> (Features,Double) -> Double -> Double ->
           Int -> Int -> m (Features,Double)
greedy' info fs f open closed best epsilon stopAt k nochange = do
  let mx = maximumBy (comparing snd) open
      open' = delete mx open
      closed' = mx : closed
      best' = if snd mx - epsilon > snd best
              then mx
              else best
      zeros = toList $ diff fs (fst mx)
      ones = toList $ (fst mx)
      children = map (union (fst mx).fromList.(:[])) zeros ++
                 map (diff (fst mx).fromList.(:[])) ones
      children' = children \\ (map fst open ++ map fst closed)
  vals <- mapM f children'
  let open'' = open' ++ zip children' vals
      nochange' = if best == best' then nochange+1 else 0
  info (show best')
  case nochange' < k && snd best' < stopAt of
    True -> greedy' info fs f open'' closed' best' epsilon stopAt k nochange'
    False -> return best'
