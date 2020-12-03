module Day02 (main, part1, part2) where

import Text.Regex.PCRE

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> (Int, Int, Char, String)
parse string = do
  let (_, _, _, min : max : char : password : _) = string =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: (String, String, String, [String])
  (read min :: Int, read max :: Int, head char, password)

validate1 :: (Int, Int, Char, String) -> Bool
validate1 (min, max, char, password) = do
  let len = length $ filter (== char) password
  len >= min && len <= max

validate2 :: (Int, Int, Char, String) -> Bool
validate2 (min, max, char, password) = do
  let first = password !! (min - 1)
  let second = password !! (max - 1)
  let len = length $ filter (== True) $ map (== char) [first, second]
  len == 1

part1 :: String -> Int
part1 = length . filter (== True) . map (validate1 . parse) . lines

part2 :: String -> Int
part2 = length . filter (== True) . map (validate2 . parse) . lines
