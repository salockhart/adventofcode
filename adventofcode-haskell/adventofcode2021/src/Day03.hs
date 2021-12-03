module Day03 (main, part1, part2) where

import Data.List (group, groupBy, sort, transpose)
import Data.List.NonEmpty (groupAllWith)
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [[Int]]
parse = map (map (\char -> read [char])) . lines

btoi :: [Int] -> Int
btoi = sum . zipWith (\i x -> x * (2 ^ i)) [0 ..] . reverse

indices :: [b] -> [Int]
indices = zipWith const [0 ..]

nBit :: Int -> [[b]] -> [b]
nBit n = map (!! n)

occurrences = map length . group . sort

mostFrequent (z : o : _)
  | z == o = 1
  | z > o = 0
  | z < o = 1
mostFrequent _ = error "cannot find most frequent"

leastFrequent = (1 -) . mostFrequent

part1 :: String -> String
part1 =
  show
    . uncurry (*)
    . solve
    . parse
  where
    solve xss = (gamma xss, epsilon xss)

    gamma = btoi . map mostFrequent . occurrences'
    epsilon = btoi . map leastFrequent . occurrences'

    occurrences' xss = map (occurrences . (`nBit` xss)) $ indices $ head xss

part2 :: String -> String
part2 =
  show
    . uncurry (*)
    . solve
    . parse
  where
    solve xss = (oxygen xss, co2 xss)

    oxygen = oxygen' 0

    oxygen' _ [] = error "should not have empty list"
    oxygen' _ [x] = btoi x
    oxygen' n xss = do
      let mostCommon = mostFrequent . occurrences $ nBit n xss
      oxygen' (n + 1) $ filter (\xs -> xs !! n == mostCommon) xss

    co2 = co2' 0

    co2' _ [] = error "should not have empty list"
    co2' _ [x] = btoi x
    co2' n xss = do
      let leastCommon = leastFrequent . occurrences $ nBit n xss
      co2' (n + 1) $ filter (\xs -> xs !! n == leastCommon) xss
