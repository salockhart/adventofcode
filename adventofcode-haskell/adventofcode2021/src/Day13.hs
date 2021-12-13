module Day13 (main, part1, part2) where

import AOC (dbgCoordMap, splitOn)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Fold a = Horizontal a | Vertical a
  deriving (Show)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [Char] -> (Set (Int, Int), [Fold Int])
parse x =
  let (coords : folds : _) = splitOn "\n\n" x
   in (parseCoords coords, parseFolds folds)
  where
    parseCoords :: String -> Set (Int, Int)
    parseCoords = Set.fromList . map ((\(x : y : _) -> (read x, read y)) . splitOn ",") . lines
    parseFolds = map (parseFold . drop 11) . lines
    parseFold :: String -> Fold Int
    parseFold ('x' : _ : x) = Horizontal (read x)
    parseFold ('y' : _ : y) = Vertical (read y)
    parseFold _ = error "cannot parse fold"

applyFold :: Set (Int, Int) -> Fold Int -> Set (Int, Int)
applyFold coords (Horizontal fx) = Set.map (\(x, y) -> (if x > fx then 2 * fx - x else x, y)) coords
applyFold coords (Vertical fy) = Set.map (\(x, y) -> (x, if y > fy then 2 * fy - y else y)) coords

part1 :: String -> String
part1 = show . solve . parse
  where
    solve (coords, fold : _) = length $ applyFold coords fold
    solve _ = error "cannot solve"

part2 :: String -> String
part2 = show . solve . parse
  where
    solve (coords, folds) =
      length $
        dbgCoordMap $
          Map.fromSet (const '#') $
            foldl applyFold coords folds
