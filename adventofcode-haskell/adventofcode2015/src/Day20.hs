module Day20 (main, part1, part2) where

import Data.List (findIndex, nub)
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1 .. intSqrt n], let (q, r) = divMod n x, r == 0]

presents :: Int -> Int
presents = (10 *) . sum . factors

presents2 :: Int -> Int
presents2 n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n

part1 :: String -> String
part1 =
  show
    . fromMaybe 0
    . (\n -> findIndex (>= n) . map presents $ [0 ..])
    . read
    . head
    . lines

part2 :: String -> String
part2 =
  show
    . fromMaybe 0
    . (\n -> findIndex (>= n) . map presents2 $ [0 ..])
    . read
    . head
    . lines
