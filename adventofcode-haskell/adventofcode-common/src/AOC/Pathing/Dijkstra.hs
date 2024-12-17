module AOC.Pathing.Dijkstra
  ( dijkstra,
  )
where

import AOC.Pathing.AStar (aStar)

dijkstra ::
  (Ord c) =>
  (c -> [(c, Int)]) -> -- expand
  c -> -- source
  (c -> Bool) -> -- target
  Maybe (Int, [[c]])
dijkstra = aStar (const 0)
