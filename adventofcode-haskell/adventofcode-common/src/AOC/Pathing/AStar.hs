module AOC.Pathing.AStar
  ( aStar,
  )
where

import AOC.Pathing.Internal (mkPaths)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.PSQueue as PSQ
import qualified Data.Set as S

aStar ::
  (Ord c) =>
  ((c, Int) -> Int) -> -- heuristic
  (c -> [(c, Int)]) -> -- expand
  c -> -- source
  (c -> Bool) -> -- target
  Maybe (Int, [[c]])
aStar heuristic expand source target =
  aStar'
    (M.fromList [(source, 0)])
    (M.fromList [(source, Nothing)])
    (PSQ.fromList [source PSQ.:-> 0])
  where
    aStar' costs prevs queue
      | PSQ.null queue = Nothing
      | target current = do
          cost <- costs M.!? current
          let paths = mkPaths prevs current
          return (cost, paths)
      | otherwise =
          let (costs', prevs', queue') = foldl relax (costs, prevs, next) (expand current)
           in aStar' costs' prevs' queue'
      where
        (current, next) = first PSQ.key $ fromJust $ PSQ.minView queue
        relax (costs'', prevs'', queue'') neighbour@(neighbourKey, neighbourWeight) =
          let newCost = costs M.! current + neighbourWeight
              oldCost = fromMaybe maxBound (costs'' M.!? neighbourKey)
           in if newCost > oldCost
                then
                  ( costs'',
                    prevs'',
                    queue''
                  )
                else
                  ( M.insert neighbourKey newCost costs'',
                    if newCost == oldCost
                      then M.adjust (<&> S.insert current) neighbourKey prevs''
                      else M.insert neighbourKey (Just (S.singleton current)) prevs'',
                    PSQ.insert neighbourKey (newCost + heuristic neighbour) queue''
                  )
