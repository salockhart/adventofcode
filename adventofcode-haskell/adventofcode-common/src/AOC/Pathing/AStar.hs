module AOC.Pathing.AStar
  ( aStar,
  )
where

import AOC.Pathing.Internal (mkPath)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.PSQueue as PSQ

-- aStar
-- function reconstruct_path(cameFrom, current)
--     total_path := {current}
--     while current in cameFrom.Keys:
--         current := cameFrom[current]
--         total_path.prepend(current)
--     return total_path

-- // A* finds a path from start to goal.
-- // h is the heuristic function. h(n) estimates the cost to reach goal from node n.
-- function A_Star(start, goal, h)
--     // The set of discovered nodes that may need to be (re-)expanded.
--     // sourcely, only the start node is known.
--     // This is usually implemented as a min-heap or priority queue rather than a hash-set.
--     openSet := {start}

--     // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from the start
--     // to n currently known.
--     cameFrom := an empty map

--     // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
--     gScore := map with default value of Infinity
--     gScore[start] := 0

--     // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
--     // how cheap a path could be from start to finish if it goes through n.
--     fScore := map with default value of Infinity
--     fScore[start] := h(start)

--     while openSet is not empty
--         // This operation can occur in O(Log(N)) time if openSet is a min-heap or a priority queue
--         current := the node in openSet having the lowest fScore[] value
--         if current = goal
--             return reconstruct_path(cameFrom, current)

--         openSet.Remove(current)
--         for each neighbor of current
--             // d(current,neighbor) is the weight of the edge from current to neighbor
--             // tentative_gScore is the distance from start to the neighbor through current
--             tentative_gScore := gScore[current] + d(current, neighbor)
--             if tentative_gScore < gScore[neighbor]
--                 // This path to neighbor is better than any previous one. Record it!
--                 cameFrom[neighbor] := current
--                 gScore[neighbor] := tentative_gScore
--                 fScore[neighbor] := tentative_gScore + h(neighbor)
--                 if neighbor not in openSet
--                     openSet.add(neighbor)

--     // Open set is empty but goal was never reached
--     return failure

aStar ::
  (Ord c) =>
  ((c, Int) -> Int) -> -- heuristic
  (c -> Map.Map c v -> [(c, Int)]) -> -- neighbours
  [c] -> -- sources
  (c -> Bool) -> -- target
  Map.Map c v -> -- graph
  (Maybe Int, Maybe [c])
aStar heuristic neighbours sources target g =
  aStar'
    (Map.fromList $ map (,0) sources)
    (Map.fromList $ map (,Nothing) sources)
    (PSQ.fromList $ map (PSQ.:-> 0) sources)
  where
    aStar' costs prevs queue
      | PSQ.null queue = (Nothing, Nothing)
      | target current = (costs Map.!? current, mkPath prevs current)
      | otherwise =
          let (costs', prevs', queue') =
                foldl
                  ( \(costs'', prevs'', queue'') neighbour@(neighbourKey, neighbourWeight) ->
                      let newCost = costs Map.! current + neighbourWeight
                       in if newCost < fromMaybe maxBound (costs'' Map.!? neighbourKey)
                            then
                              ( Map.insert neighbourKey newCost costs'',
                                Map.insert neighbourKey (Just current) prevs'',
                                PSQ.insert neighbourKey (newCost + heuristic neighbour) queue''
                              )
                            else
                              ( costs'',
                                prevs'',
                                queue''
                              )
                  )
                  (costs, prevs, next)
                  (neighbours current g)
           in aStar' costs' prevs' queue'
      where
        (current, next) = first PSQ.key $ fromJust $ PSQ.minView queue
