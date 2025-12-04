module Day04 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, neighbours8, readCoordMap)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2025 04 part1 part2

parse :: T.Text -> CoordMap Char
parse =
  M.filter (== '@')
    . readCoordMap
    . lines
    . T.unpack

removables :: CoordMap Char -> [Coord]
removables =
  M.keys
    . M.filter (< 4)
    . (\m -> M.mapWithKey (\k _ -> length $ neighbours8 k m) m)

part1 :: T.Text -> Int
part1 =
  length
    . removables
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map snd
    . takeWhile ((/= 0) . snd)
    . tail
    . iterate
      ( \(m, _) ->
          let rs = removables m
           in (foldl (flip M.delete) m rs, length rs)
      )
    . (,0)
    . parse
