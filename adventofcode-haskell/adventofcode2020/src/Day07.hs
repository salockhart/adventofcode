module Day07 (main, part1, part2) where

import AOC.Data.String (splitOn)
import Data.List (find)
import qualified Data.Set as Set
import Text.Regex.PCRE ((=~))

type Color = String

type Rule = (Color, [(Int, Color)])

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseLeaf :: Color -> (Int, Color)
parseLeaf str = do
  let (_, _, _, num : color : _) = str =~ "([0-9]+) ([a-z]+ [a-z]+)" :: (String, String, String, [String])
  let num' = read num :: Int
  (num', color)

parseLeaves :: Color -> [(Int, Color)]
parseLeaves "no other bags." = []
parseLeaves str = do
  let leaves = splitOn ", " str
  map parseLeaf leaves

parse :: String -> Rule
parse str = do
  let (_, _, _, color : [rest]) = str =~ "([a-z]+ [a-z]+) bags contain (.*)" :: (String, String, String, [String])
  (color, parseLeaves rest)

findEnclosingBags :: [Rule] -> Color -> [Color]
findEnclosingBags rules color = do
  let applicableRules = [r | r <- rules, color `elem` map snd (snd r)]
  let enclosingColors = map fst applicableRules
  let otherEncolsingColors = concatMap (findEnclosingBags rules) enclosingColors
  enclosingColors ++ otherEncolsingColors

findContainedBags :: [Rule] -> Color -> Int
findContainedBags rules color = do
  let rule = find (\r -> fst r == color) rules
  case rule of
    Nothing -> 0
    Just r -> do
      sum $ map (\p -> fst p + fst p * findContainedBags rules (snd p)) (snd r)

uniques :: [String] -> [String]
uniques = Set.toList . Set.fromList

-- part1 :: String -> Int
part1 = length . uniques . find . map parse . lines where find lines = findEnclosingBags lines "shiny gold"

-- part2 :: String -> Int
part2 = find . map parse . lines where find lines = findContainedBags lines "shiny gold"
