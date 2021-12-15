module Day15 (main, part1, part2) where

import AOC (Coord, CoordMap, dbg, dbgCoordMap, forceUnwrap, getNeighbours, parseIntoCoordMap)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.PSQueue as PSQ
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> CoordMap Int
parse = parseIntoCoordMap . map (map (\x -> read [x])) . lines

bounds coords =
  ( (maximum . map fst) coords,
    (maximum . map snd) coords
  )

-- expandMap :: CoordMap Int -> CoordMap Int
expandMap g =
  let (maxX, maxY) = bounds (Map.keys g)
   in Map.unions
        [ Map.mapKeys
            (\(a, b) -> (a + (x * (maxX + 1)), b + (y * (maxY + 1))))
            $ Map.map
              (\w -> (((w - 1) + x + y) `mod` 9) + 1)
              g
          | x <- [0 .. 4],
            y <- [0 .. 4]
        ]

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

dijkstra :: Coord -> Coord -> CoordMap Int -> CoordMap Int
dijkstra source target g =
  dijkstra'
    target
    g
    (Map.fromList [(source, 0)])
    (PSQ.singleton source 0)
  where
    dijkstra' target g dists queue
      | PSQ.null queue = dists
      | PSQ.key (forceUnwrap $ PSQ.findMin queue) == target = dists
      | otherwise =
        let next = forceUnwrap $ PSQ.findMin queue
            nextKey = PSQ.key next
            distanceToCurrent = dists Map.! nextKey
            (dists', queue'') =
              foldl
                ( \(d, q) (nc, nw) ->
                    let newDistance = distanceToCurrent + nw
                     in if distanceToCurrent < fromMaybe maxBound (d Map.!? nc)
                          then
                            ( Map.insertWith min nc newDistance d,
                              PSQ.insertWith min nc newDistance q
                            )
                          else (d, q)
                )
                (dists, PSQ.deleteMin queue)
                (Map.toList (getNeighbours g nextKey))
         in dijkstra' target g dists' queue''

solve g =
  let coords = Map.keys g
      target = bounds coords
   in dijkstra (0, 0) target g Map.! target

part1 :: String -> String
part1 =
  show
    . solve
    . parse

part2 :: String -> String
part2 =
  show
    . solve
    . expandMap
    . parse
