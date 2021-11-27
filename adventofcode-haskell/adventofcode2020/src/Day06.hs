module Day06 (main, part1, part2) where

import AOC (splitOn)
import Data.List (intersect, union)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

union' :: [String] -> String
union' = foldl1 union

intersect' :: [String] -> String
intersect' = foldl1 intersect

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

part1 :: String -> Int
part1 = sum . map (length . union') . parse

part2 :: String -> Int
part2 = sum . map (length . intersect') . parse
