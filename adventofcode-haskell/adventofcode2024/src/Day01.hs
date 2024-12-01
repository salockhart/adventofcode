module Day01 (main, part1, part2) where

import AOC (mkAoCMain, notImplemented)
import AOC.Data.List (first2)
import Data.Bifunctor (Bifunctor (second))
import Data.List (group, sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
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
    . first2
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
    . first2
    . parse
