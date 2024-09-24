module Day08 (main, part1, part2) where

import AOC.Data.String (splitOn)
import Data.List (elemIndex, find, sort)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [([String], [String])]
parse =
  map
    ( \l ->
        let (i : o : _) = splitOn " | " l
         in (map sort $ words i, map sort $ words o)
    )
    . lines

part1 :: String -> String
part1 =
  show
    . solve
    . parse
  where
    solve =
      length
        . filter (lengthIn [2, 4, 3, 7])
        . concatMap snd
    lengthIn as x = isJust $ elemIndex (length x) as

part2 :: String -> String
part2 = show . solve . parse
  where
    solve = sum . map solve'
    solve' (xs, ys) = do
      let digitOne = findWithLength 2 xs
      let digitFour = findWithLength 4 xs
      let digitSeven = findWithLength 3 xs
      let digitEight = findWithLength 7 xs

      let segmentF = head $ findWithOccurences 9 xs
      let segmentC = head $ digitOne `subtractList` [segmentF]
      let segmentA = head $ digitSeven `subtractList` [segmentC, segmentF]
      let (segmentD, segmentG) = (\(x : y : _) -> if x `elem` digitFour then (x, y) else (y, x)) $ findWithOccurences 7 xs
      let segmentE = head $ findWithOccurences 4 xs
      let segmentB = head $ ['a' .. 'g'] `subtractList` [segmentA, segmentC, segmentD, segmentE, segmentF, segmentG]

      let digitZero = sort [segmentA, segmentB, segmentC, segmentE, segmentF, segmentG]
      let digitTwo = sort [segmentA, segmentC, segmentD, segmentE, segmentG]
      let digitThree = sort [segmentA, segmentC, segmentD, segmentF, segmentG]
      let digitFive = sort [segmentA, segmentB, segmentD, segmentF, segmentG]
      let digitSix = sort [segmentA, segmentB, segmentD, segmentE, segmentF, segmentG]
      let digitNine = sort [segmentA, segmentB, segmentC, segmentD, segmentF, segmentG]

      let digits = [digitZero, digitOne, digitTwo, digitThree, digitFour, digitFive, digitSix, digitSeven, digitEight, digitNine]
      let segments = (segmentA, segmentB, segmentC, segmentD, segmentE, segmentF, segmentG)

      sum $ zipWith (\i x -> 10 ^ i * x) [0 ..] $ reverse $ map (fromJust . (`elemIndex` digits)) ys
    findWithLength n xs = fromJust $ find (\x -> length x == n) xs
    findWithOccurences n xs = filter (\s -> length (filter (\x -> s `elem` x) xs) == n) ['a' .. 'g']
    subtractList xs ys = [x | x <- xs, x `notElem` ys]
