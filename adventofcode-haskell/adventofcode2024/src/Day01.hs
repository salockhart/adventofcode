module Day01 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Tuple (fromList2)
import Data.Bifunctor (Bifunctor (second))
import Data.List (group, sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 01 part1 part2

parse :: T.Text -> [[Int]]
parse =
  transpose
    . map (map ((read :: String -> Int) . T.unpack) . T.splitOn "   ")
    . T.lines

part1 :: T.Text -> Int
part1 =
  sum
    . map (abs . uncurry (-))
    . uncurry zip
    . fromList2
    . map sort
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . (\(l, r) -> map (\l' -> l' * fromMaybe 0 (r M.!? l')) l)
    . second
      ( M.fromList
          . map (\l -> (head l, length l))
          . group
          . sort
      )
    . fromList2
    . parse
