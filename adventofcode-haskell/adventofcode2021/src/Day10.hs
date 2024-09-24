module Day10 (main, part1, part2) where

import AOC.Data.List (median)
import Data.List (sort)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

closingAtomOf :: Char -> Char
closingAtomOf '(' = ')'
closingAtomOf '[' = ']'
closingAtomOf '{' = '}'
closingAtomOf '<' = '>'
closingAtomOf x = error (x : " has no closing atom")

parse :: String -> (String, Maybe Char)
parse = foldl updateStack ([], Nothing)
  where
    updateStack ([], b) x = ([x], b)
    updateStack (h : s, b) x
      | closingAtomOf h == x = (s, b)
      | x `elem` filter (/= h) [')', ']', '}', '>'] = if isJust b then (s, b) else (s, Just x)
      | otherwise = (x : h : s, b)

part1 :: String -> String
part1 =
  show
    . sum
    . map score
    . mapMaybe (snd . parse)
    . lines
  where
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137
    score x = error (x : " cannot be scored")

part2 :: String -> String
part2 =
  show
    . fromJust
    . median
    . sort
    . map (foldl score 0 . unwind . fst)
    . filter (isNothing . snd)
    . map parse
    . lines
  where
    unwind [] = []
    unwind (x : xs) = closingAtomOf x : unwind xs
    score n ')' = n * 5 + 1
    score n ']' = n * 5 + 2
    score n '}' = n * 5 + 3
    score n '>' = n * 5 + 4
    score n x = error (x : " cannot be scored")
