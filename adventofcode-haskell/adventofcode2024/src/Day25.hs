module Day25 (main, part1, part2) where

import AOC (mkAoCMain, notImplemented)
import AOC.Data.String (splitOn)
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (group, partition, transpose)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 25 part1 part2

parse :: T.Text -> ([[Int]], [[Int]])
parse =
  bimap (heights 0) (heights 1)
    . partition (all (== '#') . head)
    . map lines
    . splitOn "\n\n"
    . T.unpack
  where
    heights n = map (map ((\x -> x - 1) . length . (!! n) . group) . transpose)

pairUp :: [[Int]] -> [[Int]] -> [([Int], [Int])]
pairUp locks keys =
  foldr (\lock -> (++) (map (lock,) keys)) [] locks

part1 :: T.Text -> Int
part1 =
  length
    . filter (all (< 6))
    . map (uncurry (zipWith (+)))
    . uncurry pairUp
    . parse

part2 :: T.Text -> ()
part2 = notImplemented
