module Day18 (main, part1, part2) where

import AOC (Coord, CoordMap, parseIntoCoordMap)
import Data.List (foldl', nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)

main :: IO ()
main = interact (show . \input -> (part1 100 input, part2 100 input))

adjacents :: Coord -> [Coord]
adjacents (x, y) =
  [(x', y') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1], (x', y') /= (x, y), x' `elem` [0 .. 99], y' `elem` [0 .. 99]]

-- double :: a -> (a, a)
-- double x = (x, x)

targetCoords :: CoordMap Char -> [Coord]
targetCoords m = do
  let coords = map fst $ Map.toList m
  let adjs = concatMap adjacents coords
  nub (coords ++ adjs)

nextValue :: CoordMap Char -> Coord -> Bool
nextValue m c = do
  let numOn = length $ filter (== '#') $ mapMaybe (m Map.!?) $ adjacents c
  let curr = fromMaybe '.' $ m Map.!? c
  case curr of
    '#' -> numOn `elem` [2, 3]
    '.' -> numOn == 3

execute :: Int -> CoordMap Char -> CoordMap Char
execute n = (!! n) . iterate applyIteration
  where
    applyIteration :: CoordMap Char -> CoordMap Char
    applyIteration m = foldl' (updateValue m) m $ targetCoords m
    updateValue :: CoordMap Char -> CoordMap Char -> Coord -> CoordMap Char
    updateValue m m' c = if nextValue m c then Map.insert c '#' m' else Map.delete c m'

execute' :: Int -> CoordMap Char -> CoordMap Char
execute' n = (!! n) . iterate applyIteration
  where
    applyIteration :: CoordMap Char -> CoordMap Char
    applyIteration m = foldl' (updateValue m) m $ filter (\c -> c `notElem` [(0, 0), (0, 99), (99, 0), (99, 99)]) $ targetCoords m
    updateValue :: CoordMap Char -> CoordMap Char -> Coord -> CoordMap Char
    updateValue m m' c = if nextValue m c then Map.insert c '#' m' else Map.delete c m'

part1 :: Int -> String -> String
part1 n =
  show
    . length
    . execute n
    . Map.filter (== '#')
    . parseIntoCoordMap
    . lines

part2 :: Int -> String -> String
part2 n =
  show
    . length
    . execute' n
    . Map.filter (== '#')
    . Map.insert (0, 0) '#'
    . Map.insert (0, 99) '#'
    . Map.insert (99, 0) '#'
    . Map.insert (99, 99) '#'
    . parseIntoCoordMap
    . lines
