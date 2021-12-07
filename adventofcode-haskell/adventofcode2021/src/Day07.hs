module Day07 (main, part1, part2) where

import AOC (median, splitOn)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text.Foreign (dropWord16)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [Int]
parse = map read . splitOn ","

part1 :: String -> String
part1 = show . solve . parse
  where
    solve xs = do
      let best = fromMaybe 0 $ (median . sort) xs
      fuelCost best xs
    fuelCost p = sum . map (\c -> abs (c - p))

part2 :: String -> String
part2 = show . solve . parse
  where
    solve cs = minimum [fuelCost p cs | p <- [minimum cs .. maximum cs]]
    fuelCost p = sum . map (\c -> let n = abs (c - p) in (n * (n + 1)) `quot` 2)
