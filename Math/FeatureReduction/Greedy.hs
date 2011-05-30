module Math.FeatureReduction.Greedy
    (
      greedy
    ) where

import Data.List (maximumBy,delete,(\\))
import Data.Ord
import Math.FeatureReduction.Features hiding (delete)

greedy :: Monad m => Features -> (Features -> m Double) ->
          Double -> Int -> m (Features,Double)
greedy fs f epsilon k = do
  let open = [(fromList [],0)]
  greedy' fs f open [] (head open) epsilon k 0

greedy' :: Monad m => Features -> (Features -> m Double) -> [(Features,Double)] ->
           [(Features,Double)] -> (Features,Double) -> Double ->
           Int -> Int -> m (Features,Double)
greedy' fs f open closed best epsilon k nochange = do
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
  case nochange' < k of
    True -> greedy' fs f open'' closed' best' epsilon k nochange'
    False -> return best'