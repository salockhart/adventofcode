module Day09 (main, part1, part2) where

import AOC.CoordMap (Coord, CoordMap, neighbours4, readCoordMap)
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> CoordMap Int
parse = readCoordMap . map (map (\c -> read [c])) . lines

getHigherNeighbours :: CoordMap Int -> (Coord, Int) -> CoordMap Int
getHigherNeighbours cm (coord, me) = Map.filter (> me) $ Map.fromList $ neighbours4 coord cm

findLowPoints :: CoordMap Int -> CoordMap Int
findLowPoints cm = Map.filterWithKey isLowPoint cm
  where
    isLowPoint coord me = do
      let neighbours = Map.fromList $ neighbours4 coord cm
      let higherNeighbours = getHigherNeighbours cm (coord, me)
      neighbours == higherNeighbours

getBasin :: Map.Map Coord Int -> (Coord, Int) -> [(Coord, Int)]
getBasin _ (_, 9) = []
getBasin cm (coord, me) = do
  let higherNeighbours = getHigherNeighbours cm (coord, me)
  (coord, me) : concatMap (getBasin cm) (Map.toList higherNeighbours)

part1 :: String -> String
part1 =
  show
    . solve
    . parse
  where
    solve =
      (\lps -> sum lps + length lps)
        . Map.elems
        . findLowPoints

part2 :: String -> String
part2 =
  show
    . solve
    . parse
  where
    solve cm = do
      let lowPoints = findLowPoints cm
      product $ take 3 $ reverse $ sort $ map (length . Map.fromList . getBasin cm) (Map.toList lowPoints)
