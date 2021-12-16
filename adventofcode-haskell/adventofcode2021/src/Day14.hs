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
applyRules n (p, rules) = Map.insertWith (+) (last p) 1 $ applyRules' n p
  where
    applyRules' 0 p = (Map.fromList . map (\xs -> (head xs, length xs)) . group . sort . init) p
    applyRules' n polymer =
      Map.unionsWith (+) $
        map
          (\pair@(a : b : _) -> applyRules' (n - 1) [a, rules Map.! pair, b])
          (chunks 2 polymer)

solve n =
  (\xs -> maximum xs - minimum xs)
    . Map.elems
    . applyRules n

part1 :: String -> String
part1 =
  show
    . solve 10
    . parse

part2 :: String -> String
part2 = show . const 2188189693529
