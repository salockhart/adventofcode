module Day11 (main, part1, part2) where

import AOC.Data.List (chunks)
import Data.Char (chr, ord)
import Data.List (find, group)
import Data.Maybe (isJust)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

inc :: Char -> Int -> Char
inc x 0 = x
inc 'z' n = 'a' `inc` (n - 1)
inc c n = chr (ord c + 1) `inc` (n - 1)

increment :: [Char] -> [Char]
increment = reverse . increment' . reverse
  where
    increment' :: [Char] -> [Char]
    increment' [] = []
    increment' (c : cs) = (c `inc` 1) : (if c == 'z' then increment' cs else cs)

isValidPassword :: [Char] -> Bool
isValidPassword pass =
  'i' `notElem` pass
    && 'o' `notElem` pass
    && 'l' `notElem` pass
    && hasRun pass
    && hasPairs pass

hasPairs :: [Char] -> Bool
hasPairs = (>= 2) . length . filter ((>= 2) . length) . group

hasRun :: [Char] -> Bool
hasRun = isJust . find isRun . chunks 3

isRun :: [Char] -> Bool
isRun [a, b, c] = a `inc` 1 == b && b `inc` 1 == c && 'z' `notElem` [a, b]

findN :: Int -> String -> String
findN n = (!! (n - 1)) . filter isValidPassword . drop 1 . iterate increment

part1 :: String -> String
part1 = show . findN 1

part2 :: String -> String
part2 = show . findN 2
