module Day02 (main, part1, part2) where

import Data.List (find)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [[Int]]
parse = map (map read . words) . lines

findEvenlyDivisible [] = error "didn't find a pair..."
findEvenlyDivisible (x : xs) = do
  let partner = find (\y -> x `mod` y == 0 || y `mod` x == 0) xs
  case partner of
    Nothing -> findEvenlyDivisible xs
    Just partner'
      | x > partner' -> (x, partner')
      | otherwise -> (partner', x)

part1 :: String -> String
part1 =
  show
    . sum
    . map (\xs -> maximum xs - minimum xs)
    . parse

part2 :: String -> String
part2 =
  show
    . sum
    . map (uncurry quot . findEvenlyDivisible)
    . parse
