module Day10 (main, part1, part2) where

import Data.Char (digitToInt, intToDigit)
import Data.List (group)

main :: IO ()
main = interact (show . \input -> (part1 40 input, part2 50 input))

execute :: Int -> [Int] -> [Int]
execute 0 nums = nums
execute round nums = execute (round - 1) (applyRound nums)
  where
    applyRound = concatMap (\g -> [length g, head g]) . group

part1 :: Int -> String -> String
part1 rounds = show . length . execute rounds . map digitToInt

part2 :: Int -> String -> String
part2 rounds = show . length . execute rounds . map digitToInt
