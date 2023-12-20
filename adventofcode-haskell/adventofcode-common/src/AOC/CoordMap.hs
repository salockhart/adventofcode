module AOC.CoordMap
  ( Coord,
    CoordMap,
    dbgCoordMap,
    diagonals,
    neighbours4,
    neighbours8,
    readCoordMap,
  )
where

import Data.List (foldl', union)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

type Coord = (Int, Int)

type CoordMap a = Map.Map Coord a

readCoordMap :: [[a]] -> CoordMap a
readCoordMap =
  foldl'
    ( \m (y, line) ->
        Map.union
          m
          ( foldl'
              (\m' (x, char) -> Map.insert (x, y) char m')
              Map.empty
              $ zip [0 ..] line
          )
    )
    Map.empty
    . zip [0 ..]

lookupMany :: CoordMap a -> [Coord] -> [(Coord, a)]
lookupMany m = mapMaybe (\c -> Map.lookup c m >>= (\value -> Just (c, value)))

neighbours4 :: CoordMap a -> Coord -> [(Coord, a)]
neighbours4 m (x, y) =
  lookupMany m [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

diagonals :: CoordMap a -> Coord -> [(Coord, a)]
diagonals m (x, y) =
  lookupMany m [(x - 1, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1)]

neighbours8 :: (Eq a, Show a) => CoordMap a -> Coord -> [(Coord, a)]
neighbours8 m coord = trace ("neighbours4=" ++ show (neighbours4 m coord)) (neighbours4 m coord) `union` trace ("diagonals=" ++ show (diagonals m coord)) (diagonals m coord)

dbgCoordMap :: Show a => CoordMap a -> CoordMap a
dbgCoordMap m =
  trace
    ( concat
        [ ( concat
              [ getValue x y ++ " "
                | x <- [minX .. maxX]
              ]
          )
            ++ "\n"
          | y <- [minY .. maxY]
        ]
    )
    m
  where
    coords = Map.keys m
    xs = map fst coords
    ys = map snd coords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    getValue x y = maybe " " show (m Map.!? (x, y))
