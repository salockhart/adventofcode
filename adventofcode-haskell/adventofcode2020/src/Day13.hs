module Day13 (main, part1, part2) where

import AOC.Data.String (splitOn)
import qualified Data.Bifunctor as Bifunctor
import Data.List (sort)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [String] -> (Int, [Int])
parse [earliest, ids] = (read earliest, sort $ map read $ filter (/= "x") $ splitOn "," ids)

parse' :: [String] -> [(Int, Int)]
parse' [_, ids] = map (Bifunctor.second read) $ filter (\(_, c) -> c /= "x") $ zip [0 ..] $ splitOn "," ids

findEarliestBus :: (Int, [Int]) -> (Int, Int, Int)
findEarliestBus (time, ids) = do
  head $ filter isGoodPair $ head $ filter (any isGoodPair) [[(id, i, i `mod` id) | id <- ids] | i <- [time ..]]
  where
    isGoodPair (_, _, remainder) = remainder == 0

part1 :: String -> String
part1 = show . solve . parse . lines
  where
    solve (time, ids) = do
      let (id, time', _) = findEarliestBus (time, ids)
      id * (time' - time)

part2 :: String -> String
part2 = show . head . solve . parse' . lines
  where
    solve :: [(Int, Int)] -> [Int]
    solve routes = do
      let first = snd (head routes)
      solve' (first, first) routes

    solve' :: (Int, Int) -> [(Int, Int)] -> [Int]
    solve' (start, _) [] = [start]
    solve' (start, period) ((idx, route) : rest) = do
      let [first, second] = take 2 [i | i <- [start, start + period ..], (i + idx) `mod` route == 0]
      solve' (first, second - first) rest
