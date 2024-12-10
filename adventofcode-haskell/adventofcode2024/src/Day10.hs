module Day10 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, neighbours4, readCoordMap)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 10 part1 part2

parse :: T.Text -> CoordMap Int
parse =
  M.map ((read :: String -> Int) . (: []))
    . readCoordMap
    . lines
    . T.unpack

findTrailheads :: CoordMap Int -> [[[Coord]]]
findTrailheads cm =
  map (filter (\ns -> (cm M.! last ns) == 9) . findTrailheads') $
    M.toList $
      M.filter (== 0) cm
  where
    findTrailheads' :: (Coord, Int) -> [[Coord]]
    findTrailheads' (sc, sh) =
      case filter (\(_, h) -> (h - sh) == 1) $ neighbours4 sc cm of
        [] -> [[sc]]
        ns -> concatMap (map (sc :) . findTrailheads') ns

part1 :: T.Text -> Int
part1 =
  sum
    . map (length . S.fromList . map last)
    . findTrailheads
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map length
    . findTrailheads
    . parse
