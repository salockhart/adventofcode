module Day17 (main, part1, part2) where

import AOC.Data.List (combinations)
import Data.List (sortOn)

main :: IO ()
main = interact (show . \input -> (part1 150 input, part2 150 input))

part1 :: Int -> String -> String
part1 n =
  show
    . length
    . filter ((== n) . sum)
    . combinations
    . (map read :: [String] -> [Int])
    . lines

part2 :: Int -> String -> String
part2 n =
  show
    . length
    . (\cs -> takeWhile ((== (length $ head cs)) . length) cs)
    . sortOn length
    . filter ((== n) . sum)
    . combinations
    . (map read :: [String] -> [Int])
    . lines
