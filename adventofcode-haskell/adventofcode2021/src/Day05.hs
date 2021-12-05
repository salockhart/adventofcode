{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05 (main, part1, part2) where

import AOC (splitOn)
import Data.List (group, sort)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [[[Int]]]
parse = map (map (map read . splitOn ",") . splitOn " -> ") . lines

expand :: [[[Int]]] -> [(Int, Int)]
expand =
  concatMap getPoints
  where
    getPoints line
      | isNotDiagonal line = getHorizontalLine line
      | otherwise = getDiagonalLine line
    getHorizontalLine ((x1 : y1 : _) : (x2 : y2 : _) : _) =
      [ (x, y)
        | x <- [minimum [x1, x2] .. maximum [x1, x2]],
          y <- [minimum [y1, y2] .. maximum [y1, y2]]
        ]
    getDiagonalLine ((x1 : y1 : _) : (x2 : y2 : _) : _) = do
      let slope = quot (y2 - y1) (x2 - x1)
      let intersect = y2 - slope * x2
      [(x, (slope * x) + intersect)
        | x <- [minimum [x1, x2] .. maximum [x1, x2]]
        ]

isNotDiagonal :: [[Int]] -> Bool
isNotDiagonal ((x1 : y1 : _) : (x2 : y2 : _) : _) =
  x1 == x2 || y1 == y2

noDiagonal :: [[[Int]]] -> [[[Int]]]
noDiagonal = filter isNotDiagonal

findNumDangerous :: [(Int, Int)] -> Int
findNumDangerous =
  length
    . filter (>= 2)
    . map length
    . group
    . sort

part1 :: String -> String
part1 =
  show
    . findNumDangerous
    . expand
    . noDiagonal
    . parse

part2 :: String -> String
part2 =
  show
    . findNumDangerous
    . expand
    . parse
