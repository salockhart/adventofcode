module AOC.Pathing.Dijkstra
  ( dijkstra,
  )
where

import AOC.Pathing.Internal (mkPath)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.PSQueue as PSQ

-- dijkstra
-- 1  function Dijkstra(Graph, source):
-- 2      dist[source] ← 0                           // Initialization
-- 3
-- 4      create vertex priority queue Q
-- 5
-- 6      for each vertex v in Graph:
-- 7          if v ≠ source
-- 8              dist[v] ← INFINITY                 // Unknown distance from source to v
-- 9              prev[v] ← UNDEFINED                // Predecessor of v
-- 10
-- 11         Q.add_with_priority(v, dist[v])
-- 12
-- 13
-- 14     while Q is not empty:                      // The main loop
-- 15         u ← Q.extract_min()                    // Remove and return best vertex
-- 16         for each neighbor v of u:              // only v that are still in Q
-- 17             alt ← dist[u] + length(u, v)
-- 18             if alt < dist[v]
-- 19                 dist[v] ← alt
-- 20                 prev[v] ← u
-- 21                 Q.decrease_priority(v, alt)
-- 22
-- 23     return dist, prev

dijkstra ::
  (Ord c) =>
  (c -> Map.Map c v -> [(c, Int)]) -> -- neighbours
  [c] -> -- sources
  (c -> Bool) -> -- target
  Map.Map c v -> -- graph
  (Maybe Int, Maybe [c])
dijkstra neighbours sources target g =
  dijkstra'
    (Map.fromList $ map (,0) sources)
    (Map.fromList $ map (,Nothing) sources)
    (PSQ.fromList $ map (PSQ.:-> 0) sources)
  where
    dijkstra' costs prevs queue
      | PSQ.null queue = (Nothing, Nothing)
      | target current = (costs Map.!? current, mkPath prevs current)
      | otherwise =
          let (costs', prevs', queue') =
                foldl
                  ( \(costs'', prevs'', queue'') (neighbourKey, neighbourWeight) ->
                      let newCost = costs Map.! current + neighbourWeight
                       in if newCost < fromMaybe maxBound (costs'' Map.!? neighbourKey)
                            then
                              ( Map.insert neighbourKey newCost costs'',
                                Map.insert neighbourKey (Just current) prevs'',
                                PSQ.insert neighbourKey newCost queue''
                              )
                            else
                              ( costs'',
                                prevs'',
                                queue''
                              )
                  )
                  (costs, prevs, next)
                  (neighbours current g)
           in dijkstra' costs' prevs' queue'
      where
        (current, next) = first PSQ.key $ fromJust $ PSQ.minView queue
