module Day05 (main, part1, part2) where

import Data.List

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

groupInto2 :: String -> [(Char, Char)]
groupInto2 str = [(str !! i, str !! (i + 1)) | i <- [0 .. length str - 2]]

threeVowels :: String -> Bool
threeVowels str = do
  let vowels = filter (`elem` "aeiou") str
  length vowels >= 3

adjacents :: String -> Bool
adjacents = any (uncurry (==)) . groupInto2

badPairs :: String -> Bool
badPairs str =
  not
    ( isInfixOf "ab" str
        || isInfixOf "cd" str
        || isInfixOf "pq" str
        || isInfixOf "xy" str
    )

splitPair :: String -> Bool
splitPair [] = False
splitPair [_] = False
splitPair [_, _] = False
splitPair [a, _, c] = a == c
splitPair (a : b : c : rest) = splitPair [a, b, c] || splitPair (b : c : rest)

doublePair :: String -> Bool
doublePair [] = False
doublePair [_] = False
doublePair [_, _] = False
doublePair (a : b : rest) = pair (a, b) rest || doublePair (b : rest)
  where
    pair (c, d) s = elem (c, d) $ groupInto2 s

part1 :: String -> Int
part1 =
  length
    . filter (== True)
    . map (\line -> threeVowels line && adjacents line && badPairs line)
    . lines

-- part2 :: String -> Int
part2 =
  length
    . filter (== True)
    . map (\line -> splitPair line && doublePair line)
    . lines
