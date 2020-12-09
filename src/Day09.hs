module Day09 (main, part1, part2) where

import Data.List (find)

main :: IO ()
main = interact (show . \input -> (part1 25 input, part2 25 input))

windows :: Int -> [Int] -> [[Int]]
windows size strs = [take (size + 1) (drop i strs) | i <- [0 .. (length strs - size)]]

splitTargetAndPreamble :: [Int] -> ([Int], Int)
splitTargetAndPreamble strs = (init strs, last strs)

hasPair :: ([Int], Int) -> Bool
hasPair ([], _) = False
hasPair (head : preamble, target) = do
  let counterpart = find (\i -> i + head == target) preamble
  case counterpart of
    Nothing -> hasPair (preamble, target)
    Just _ -> True

parse :: [String] -> [Int]
parse = map read

part1 :: Int -> String -> Int
part1 size = head . map snd . filter (not . hasPair) . map splitTargetAndPreamble . windows size . parse . lines

findSubsetSummingTo :: Int -> [Int] -> [Int]
findSubsetSummingTo _ [] = []
findSubsetSummingTo target list = do
  let sets = filter (\set -> sum set == target) [take i list | i <- [1 .. length list]]
  if null sets
    then findSubsetSummingTo target (tail list)
    else head sets

part2 :: Int -> String -> Int
part2 size input = do
  let input' = parse $ lines input
  let sum = part1 size input
  let subset = findSubsetSummingTo sum input'
  let min = minimum subset
  let max = maximum subset
  min + max
