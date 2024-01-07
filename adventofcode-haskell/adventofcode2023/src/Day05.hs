module Day05 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.List (sortOn)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

main :: IO ()
main = mkAoCMain 2023 05 part1 part2

parse :: T.Text -> ([Int], [[(Int, Int, Int)]])
parse = parse' . filter (not . T.null) . T.lines
  where
    parse' (seeds : maps) = (parseSeeds seeds, parseMaps maps)
    parse' _ = error "bad pattern"
    parseIntList =
      map (fst . fromRight (error "failed to parse") . TR.decimal)
        . T.splitOn " "
    parseSeeds =
      parseIntList
        . (!! 1)
        . T.splitOn "seeds: "
    parseMaps =
      (\(m, rest) -> sortOn fstOf3 (map (toRange . parseIntList) m) : if null rest then [] else parseMaps rest)
        . span (\x -> not $ ':' `T.elem` x)
        . tail
      where
        toRange (d : s : l : _) = (s, s + l - 1, d - s)
        toRange _ = error "failed to make range"
        fstOf3 (a, _, _) = a

solve :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
solve = foldl (\seeds ms -> concatMap (applyMaps ms) seeds)

applyMaps :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
applyMaps [] seed = [seed]
applyMaps ((s, e, shift) : ms) seed@(seedStart, seedEnd)
  | seedStart > e || seedEnd < s =
      applyMaps ms seed
  | s <= seedStart && seedEnd <= e =
      [(seedStart + shift, seedEnd + shift)]
  | s <= seedStart && e < seedEnd =
      (seedStart + shift, e + shift) : applyMaps ms (e + 1, seedEnd)
  | seedStart < s && seedEnd <= e =
      [(seedStart, s - 1), (s + shift, seedEnd + shift)]
  | seedStart < s && e < seedEnd =
      [(seedStart, s - 1), (s + shift, e + shift)] ++ applyMaps ms (e + 1, seedEnd)
  | otherwise = error "unhandled case in applyMaps"

part1 :: T.Text -> Int
part1 = minimum . map fst . uncurry solve . first modifySeeds . parse
  where
    modifySeeds (a : seeds) = (a, a) : modifySeeds seeds
    modifySeeds [] = []

part2 :: T.Text -> Int
part2 = minimum . map fst . uncurry solve . first modifySeeds . parse
  where
    modifySeeds (a : b : rest) = (a, a + b - 1) : modifySeeds rest
    modifySeeds [_] = error "should not get here, odd number of seeds"
    modifySeeds [] = []
