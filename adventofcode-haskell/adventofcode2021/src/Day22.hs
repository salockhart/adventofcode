module Day22 (main, part1, part2) where

import AOC (dbg)
import Data.List (foldl', sort)
import Data.List.Split (splitOn)
import Data.Set (Set, difference, empty, fromList, union)
import Debug.Trace (trace)

type Coord3 = (Int, Int, Int)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [(String, Set Coord3)]
parse = map (parse' . words) . lines
  where
    parse' ("on" : coords : _) = parseCoords "on" coords
    parse' ("off" : coords : _) = parseCoords "off" coords
    parse' _ = error "cannot parse"
    parseCoords n cs = do
      let (x : y : z : _) = splitOn "," cs
      let (minX, maxX) = parseCoord x
      let (minY, maxY) = parseCoord y
      let (minZ, maxZ) = parseCoord z
      ( n,
        fromList
          [ (x', y', z')
            | x' <- [minX .. maxX],
              y' <- [minY .. maxY],
              z' <- [minZ .. maxZ]
          ]
        )
    parseCoord =
      (\(min : max : _) -> (read min, read max))
        . splitOn ".."
        . drop 2

applyStep :: Set Coord3 -> (String, Set Coord3) -> Set Coord3
applyStep coords ("on", coords') = coords `union` coords'
applyStep coords ("off", coords') = coords `difference` coords'
applyStep coords _ = coords

part1 :: String -> String
part1 =
  show
    . length
    . foldl' applyStep empty
    . filter (\(_, coords) -> all (\(x, y, z) -> x <= 50 && y <= 50 && z <= 50) coords)
    . parse

part2 :: String -> String
part2 = show . const 1
