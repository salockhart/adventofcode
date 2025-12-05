module Day05 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.String (splitOn)
import AOC.Data.Tuple (both, fromList2)
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.List (sort)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 05 part1 part2

parse :: T.Text -> ([(Int, Int)], [Int])
parse =
  bimap
    ( map
        ( both (read :: String -> Int)
            . fromList2
            . splitOn "-"
        )
    )
    (map (read :: String -> Int))
    . fromList2
    . map lines
    . splitOn "\n\n"
    . T.unpack

part1 :: T.Text -> Int
part1 =
  length
    . (\(rs, xs) -> filter (\x -> any (\r -> r x) rs) xs)
    . first (map (\(a, b) n -> n >= a && n <= b))
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map (\(a, b) -> b - a + 1)
    . (\rs -> foldl combineRange [head rs] (tail rs))
    . sort
    . fst
    . parse
  where
    combineRange (a@(al, ar) : rest) b@(bl, br)
      -- no overlap
      | bl > ar = b : a : rest
      -- b overlaps a
      | bl <= ar && br >= ar = (al, br) : rest
      -- b is within a
      | bl <= ar && br <= ar = a : rest
      -- don't need to handle other cases thanks to us sorting first
      | otherwise = a : rest
    combineRange _ _ = error "this should not be possible"
