module Day22 (main, part1, part2) where

import AOC (mkAoCMain, notImplemented)
import AOC.Data.List (maximumOn, windows)
import Data.Bits (xor)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 22 part1 part2

parse :: T.Text -> [Int]
parse = map read . lines . T.unpack

evolve :: Int -> Int
evolve a =
  let ops = [(* 64), (`div` 32), (* 2048)]
   in foldl (\x op -> prune $ mix x $ op x) a ops
  where
    prune x = x `mod` 16777216
    mix x y = x `xor` y

priceSequences :: Int -> [([Int], Int)]
priceSequences =
  (\xs -> map collapseWindow $ windows 4 $ zipWith (\a b -> (a - b, a)) (tail xs) xs)
    . map (`mod` 10)
    . take 2001
    . iterate evolve
  where
    collapseWindow [(a, _), (b, _), (c, _), (d, p)] = ([a, b, c, d], p)
    collapseWindow _ = notImplemented

bestSequence :: [[([Int], Int)]] -> ([Int], Int)
bestSequence =
  maximumOn snd
    . M.toList
    . foldl1 (M.unionWith (+))
    . map (M.fromList . reverse)

part1 :: T.Text -> Int
part1 =
  sum
    . map ((!! 2000) . iterate evolve)
    . parse

part2 :: T.Text -> Int
part2 =
  snd
    . bestSequence
    . map priceSequences
    . parse
