module Day10 (main, part1, part2) where

import Data.List (delete, foldl', groupBy, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> Int
parse = read

findDifferences :: [Int] -> [Int]
findDifferences [] = []
findDifferences [_] = []
findDifferences [a, b] = [abs b - a]
findDifferences (a : b : rest) = do
  findDifferences [a, b] ++ findDifferences (b : rest)

group :: [Int] -> Map.Map Int Int
group = foldl' (\m i -> Map.insertWith (+) i 1 m) Map.empty

addEnds :: [Int] -> [Int]
addEnds arr = 0 : arr ++ [last arr + 3]

groupInto3 :: [Int] -> [(Int, Int, Int)]
groupInto3 [] = []
groupInto3 [_, _] = []
groupInto3 str = [(str !! i, str !! (i + 1), str !! (i + 2)) | i <- [0 .. length str - 3]]

findOptionals :: [Int] -> [Int]
findOptionals = map (\(_, x, _) -> x) . filter (\(a, _, c) -> c - a < 4) . groupInto3

validArrangement :: [Int] -> Bool
validArrangement arrangement = do
  let differences = filter (> 3) $ findDifferences arrangement
  null differences

findArrangements :: [Int] -> [[Int]]
findArrangements jolts = do
  let rest = concat [findArrangements $ delete optional jolts | optional <- findOptionals jolts]
  if validArrangement jolts
    then jolts : rest
    else rest

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList

chunk :: [Int] -> [[Int]]
chunk =
  map reverse
    . reverse
    . foldl'
      ( \chunks num -> do
          if null chunks
            then [[num]]
            else do
              let lastChunk = head chunks
              let lastItem = head lastChunk
              if num - lastItem < 3
                then (num : lastChunk) : tail chunks
                else [num] : chunks
      )
      []

part1 :: String -> Int
part1 = calculate . group . findDifferences . addEnds . sort . map parse . lines
  where
    calculate m = fromMaybe 0 (m Map.!? 1) * fromMaybe 0 (m Map.!? 3)

-- part2 :: String -> Int
part2 = show . product . map (length . uniques . findArrangements) . chunk . addEnds . sort . map parse . lines
