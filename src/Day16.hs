module Day16 (main, part1, part2) where

import AOC (splitOn)
import qualified Control.Arrow as Data.Bifunctor
import Data.List (isPrefixOf, sortBy)
import Text.Regex.PCRE ((=~))

type Rule = (String, [Int])

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseTickets :: [String] -> [[Int]]
parseTickets = map (map read . splitOn ",")

parseRules :: [String] -> [Rule]
parseRules = map parseRule
  where
    parseRule :: String -> Rule
    parseRule str = do
      let (_, _, _, field : lowerBound1 : upperBound1 : lowerBound2 : upperBound2 : _) = str =~ "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: (String, String, String, [String])
      let lowerBound1' = read lowerBound1 :: Int
      let upperBound1' = read upperBound1 :: Int
      let lowerBound2' = read lowerBound2 :: Int
      let upperBound2' = read upperBound2 :: Int
      (field, [lowerBound1' .. upperBound1'] ++ [lowerBound2' .. upperBound2'])

parse :: [String] -> ([Rule], [Int], [[Int]])
parse strs = do
  let (rules, rest) = span (/= "") strs
  let (mine, rest') = span (/= "") $ drop 1 rest
  let (nearby, _) = span (/= "") $ drop 1 rest'
  (parseRules rules, head $ parseTickets $ drop 1 mine, parseTickets $ drop 1 nearby)

findErrorRate :: ([Rule], [Int], [[Int]]) -> Int
findErrorRate (rules, _, nearby) = findErrorRate' (concatMap snd rules) (concat nearby)
  where
    findErrorRate' allowedValues nearbyValues = sum $ filter (`notElem` allowedValues) nearbyValues

filterValidTickets :: ([Rule], [Int], [[Int]]) -> ([Rule], [Int], [[Int]])
filterValidTickets (rules, mine, nearby) = do
  let allowedValues = concatMap snd rules
  (rules, mine, filter (all (`elem` allowedValues)) nearby)

findPossibleFields :: [Rule] -> [[Int]] -> [[String]]
findPossibleFields rules nearby =
  [map fst $ filter (\(_, vals) -> all ((`elem` vals) . (!! i)) nearby) rules | i <- [0 .. length (head nearby) - 1]]

solveFields :: [(Int, [String])] -> [(Int, String)]
solveFields [] = []
solveFields fields = do
  let fields' = sortBy (\(_, a) (_, b) -> compare (length a) (length b)) fields
  let solved = Data.Bifunctor.second head $ head fields'
  let filtered = map (Data.Bifunctor.second (filter (/= snd solved))) (tail fields')
  solved : solveFields filtered

part1 :: String -> String
part1 = show . findErrorRate . parse . lines

part2 :: String -> String
part2 = show . solve . filterValidTickets . parse . lines
  where
    solve (rules, mine, nearby) =
      product $
        map ((mine !!) . fst) $
          filter (\(_, f) -> "departure" `isPrefixOf` f) $
            solveFields $
              zip [0 ..] $
                findPossibleFields rules nearby
