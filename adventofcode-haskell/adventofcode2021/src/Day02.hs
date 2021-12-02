module Day02 (main, part1, part2) where

import Data.List (foldl')

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> [[String]]
parse = map words . lines

solve :: [[[Char]]] -> (Int, Int)
solve = foldl' parseInstr (0, 0)
  where
    parseInstr (h, d) ("forward" : x : _) = (h + read x, d)
    parseInstr (h, d) ("down" : x : _) = (h, d + read x)
    parseInstr (h, d) ("up" : x : _) = (h, d - read x)
    parseInstr acc _ = acc

solve' :: [[[Char]]] -> (Int, Int, Int)
solve' = foldl' parseInstr (0, 0, 0)
  where
    parseInstr (h, d, a) ("forward" : x : _) = (h + read x, d + read x * a, a)
    parseInstr (h, d, a) ("down" : x : _) = (h, d, a + read x)
    parseInstr (h, d, a) ("up" : x : _) = (h, d, a - read x)
    parseInstr acc _ = acc

part1 :: String -> String
part1 =
  show
    . uncurry (*)
    . solve
    . parse

part2 :: String -> String
part2 =
  show
    . (\(h, d, _) -> h * d)
    . solve'
    . parse
