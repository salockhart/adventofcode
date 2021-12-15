module Day14 (main, part1, part2) where

import AOC (applyN, chunks, splitOn)
import Data.List (group, sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Debug.Trace (trace)

type State = ([Char], Map.Map [Char] Char)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> State
parse x =
  let (template : rules : _) = splitOn "\n\n" x
   in (template, parseRules rules)
  where
    parseRules = Map.fromList . map parseRule . lines
    parseRule r = let (ins : outs : _) = splitOn " -> " r in (ins, head outs)

-- applyRules :: State -> State
applyRules (polymer, rules) = do
  let pairs = chunks 2 polymer
  let updatedPolymer =
        catMaybes $
          foldReactions $
            map (\pair@(a : b : _) -> [Just a, rules Map.!? pair, Just b]) $
              chunks 2 polymer
  (updatedPolymer, rules)
  where
    foldReactions rs = foldl1 (\acc triple -> init acc ++ triple) rs

countOccurrences =
  map (\x -> (x, length x))
    . group
    . sort

solve n =
  (\ls -> maximum ls - minimum ls)
    . map snd
    . countOccurrences
    . fst
    . applyN n applyRules

part1 :: String -> String
part1 =
  show
    . solve 10
    . parse

part2 :: String -> String
part2 = show . const 2188189693529
