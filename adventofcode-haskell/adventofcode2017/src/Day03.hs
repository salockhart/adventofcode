module Day03 (main, part1, part2) where

import Data.Complex (magnitude)
import Data.Function (on)
import Data.List (findIndex, minimumBy)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> Int
parse = read . head . lines

-- the distance between corners goes up by 8 each time
-- 101                                       91
--      65  64  63  62  61  60  59  58  57
--      66  37  36  35  34  33  32  31  56
--      67  38  17  16  15  14  13  30  55
--      68  39  18   5   4   3  12  29  54
--      69  40  19   6   1   2  11  28  53
--      70  41  20   7   8   9  10  27  52
--      71  42  21  22  23  24  25  26  51
--      72  43  44  45  46  47  48  49  50   83
--      73  74  75  76  77  78  79  80  81   82
-- 111                                      121

-- 1 -> 3 -> 13 -> 31
--    2 +8 10 +8 18

-- or...

-- (1n + 8)  (1n + 7)  (1n + 6)  (1n + 5)  (1n + 4)
-- (1n + 9)  (0n + 4)  (0n + 3)  (0n + 2)  (1n + 3)
-- (1n + 10) (0n + 5)     (0n)   (0n + 1)  (1n + 2)
-- (1n + 11) (0n + 6)  (0n + 7)  (1n    )  (1n + 1)
-- (1n + 12) (1n + 13) (1n + 14) (1n + 15) (3n    )
--                                                   (6n    )
--                                                             (10n    )
-- where n = 8

-- to solve:
-- 1. which ring are you in? (magnitude)
-- 2. which direction are you in? (angle)
-- 3. convert polar to cartesian

divide' :: Int -> Int -> Float
divide' = (/) `on` fromIntegral

-- solve :: Int -> (Int, Float)
solve 0 = (0, 0)
solve n = do
  -- every bottom right corner
  let ringMaxes = 0 : [8 * x + 8 * 2 ^ x | x <- [0 ..]]

  -- the ring that the value is found in
  let magnitude = (fromMaybe 0 . findIndex (n <=)) ringMaxes
  let lowerBound = ringMaxes !! (magnitude - 1)
  let upperBound = ringMaxes !! magnitude

  let remainder = n - lowerBound

  let sideLength = (upperBound - lowerBound) `quot` 4
  let poles = [lowerBound + (sideLength `quot` 2) + x * sideLength | x <- [0 .. 3]]

  let distanceToNearestPole = minimum $ map (\x -> abs (n - x)) poles

  trace (show (magnitude, remainder, lowerBound, upperBound, poles, distanceToNearestPole)) (magnitude, distanceToNearestPole)

part1 :: String -> String
part1 = show . uncurry (+) . solve . subtract 1 . parse

part2 :: String -> String
part2 = show . const 1
