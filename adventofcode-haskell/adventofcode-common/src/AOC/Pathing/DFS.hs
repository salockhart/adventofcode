module AOC.Pathing.DFS (dfs) where

dfs ::
  (Eq a) =>
  (a -> [a]) -> -- expand
  a -> -- source
  (a -> Bool) -> -- target
  [[a]]
dfs expand source target = dfs' [] source
  where
    dfs' visited current
      | target current = [current : visited]
      | current `elem` visited = []
      | otherwise =
          let neighbours = expand current
           in concatMap (dfs' (current : visited)) neighbours
