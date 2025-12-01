module Day01 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.List (windows)
import AOC.Data.Tuple (fromList2)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 01 part1 part2

parse :: [Char] -> Int
parse ('L' : r) = -(read r)
parse ('R' : r) = read r
parse _ = error "bad pattern for turn"

part1 :: T.Text -> Int
part1 =
  length
    . filter (== 0)
    . map (`mod` 100)
    . scanl (+) 50
    . map parse
    . lines
    . T.unpack

part2 :: T.Text -> Int
part2 =
  length
    . filter (== 0)
    . map (`mod` 100)
    -- naive approach
    -- just generate all the ticks, not just stop/end spots
    -- when we concat the lists of ticks, remove dupes where they meet
    . foldl1 (\as bs -> init as ++ bs)
    . map (uncurry steps . fromList2)
    . windows 2
    --
    . scanl (+) 50
    . map parse
    . lines
    . T.unpack
  where
    steps a b
      | a > b = reverse [b .. a]
      | otherwise = [a .. b]