{-# LANGUAGE OverloadedStrings #-}

module Day05 (main, part1, part2) where

import AOC (solveAoCDay)
import Data.Either (fromRight)
import Data.Ix (Ix (inRange))
import qualified Data.Text as T
import qualified Data.Text.Read as TR

main :: IO ()
main = solveAoCDay 2023 05 part1 part2

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
      (\(m, rest) -> map (toRange . parseIntList) m : if null rest then [] else parseMaps rest)
        . span (\x -> not $ ':' `T.elem` x)
        . tail
      where
        toRange (d : s : l : _) = (s, s + l - 1, d - s)
        toRange _ = error "failed to make range"

solve :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
solve = foldl (\seeds ms -> concatMap (applyMaps ms) seeds)

applyMaps :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
applyMaps [] seed = [seed]
applyMaps ((s, e, shift) : ms) seed@(seedStart, seedEnd)
  | leftInBounds && rightInBounds =
      [(seedStart + shift, seedEnd + shift)]
  | leftInBounds && not rightInBounds =
      [(seedStart + shift, e + shift), (e + 1, seedEnd)]
  | not leftInBounds && rightInBounds =
      [(seedStart, s - 1), (s + shift, seedEnd + shift)]
  | otherwise =
      applyMaps ms seed
  where
    inRange' = inRange (s, e)
    leftInBounds = inRange' seedStart
    rightInBounds = inRange' seedEnd

part1 :: T.Text -> Int
part1 = minimum . map fst . uncurry solve . modifySeeds . parse
  where
    modifySeeds (seeds, ms) = (modifySeeds' seeds, ms)
    modifySeeds' (a : seeds) = (a, a) : modifySeeds' seeds
    modifySeeds' [] = []

part2 :: T.Text -> Int
part2 = minimum . map fst . uncurry solve . modifySeeds . parse
  where
    modifySeeds (seeds, ms) = (modifySeeds' seeds, ms)
    modifySeeds' (a : b : rest) = (a, a + b - 1) : modifySeeds' rest
    modifySeeds' [_] = error "should not get here, odd number of seeds"
    modifySeeds' [] = []
