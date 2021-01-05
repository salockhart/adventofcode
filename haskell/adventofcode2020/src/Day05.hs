module Day05 (main, part1, part2) where

import Data.List (sort)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

upperHalf :: (Int, Int) -> (Int, Int)
upperHalf (min, max) = (((min + max) `div` 2) + 1, max)

lowerHalf :: (Int, Int) -> (Int, Int)
lowerHalf (min, max) = (min, (min + max) `div` 2)

findRow :: (Int, Int) -> String -> Int
findRow pair ('B' : rest) = findRow (upperHalf pair) rest
findRow pair ('F' : rest) = findRow (lowerHalf pair) rest
findRow (min, _) ('L' : _) = min
findRow (min, _) ('R' : _) = min
findRow (min, _) [] = min

findColumn :: (Int, Int) -> String -> Int
findColumn pair ('R' : rest) = findColumn (upperHalf pair) rest
findColumn pair ('L' : rest) = findColumn (lowerHalf pair) rest
findColumn pair (_ : rest) = findColumn pair rest
findColumn (min, _) [] = min

findId :: String -> Int
findId str = do
  let row = findRow (0, 127) str
  let column = findColumn (0, 7) str
  (row * 8) + column

findSeat :: [Int] -> Int
findSeat arr = do
  let first : second : _ = arr
  if first + 2 == second
    then first + 1
    else findSeat (tail arr)

part1 :: String -> Int
part1 = maximum . map findId . lines

part2 :: String -> Int
part2 = findSeat . sort . map findId . lines
