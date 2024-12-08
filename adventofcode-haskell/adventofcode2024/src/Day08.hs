module Day08 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, readCoordMap)
import AOC.Data.List (groupOn)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 08 part1 part2

parse :: T.Text -> CoordMap Char
parse = readCoordMap . map T.unpack . T.lines

solve :: (Ord b, Show b) => (M.Map b Char -> [b] -> [b]) -> M.Map b Char -> S.Set b
solve p m =
  ( S.fromList
      . concatMap (filter (`M.member` m) . p m . map fst)
      . groupOn snd
      . sortOn snd
      . M.toList
      . M.filter (/= '.')
  )
    m

antinodes :: (t -> a1 -> a1 -> [a2]) -> t -> [a1] -> [a2]
antinodes _ _ [] = []
antinodes f m (c : cs) = do
  let as = concatMap (f m c) cs
  as ++ antinodes f m cs

part1 :: T.Text -> Int
part1 = length . solve (antinodes antinodes') . parse
  where
    antinodes' :: CoordMap a -> Coord -> Coord -> [Coord]
    antinodes' m (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
       in filter (`M.member` m) [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

part2 :: T.Text -> Int
part2 = length . solve (antinodes antinodes') . parse
  where
    antinodes' :: CoordMap a -> Coord -> Coord -> [Coord]
    antinodes' m (x1, y1) (x2, y2) =
      let dx = x2 - x1
          dy = y2 - y1
       in do
            let prev = scanl (\(x, y) _ -> (x - dx, y - dy)) (x1, y1) [1 :: Int ..]
            let next = scanl (\(x, y) _ -> (x + dx, y + dy)) (x2, y2) [1 :: Int ..]
            takeWhile (`M.member` m) prev ++ takeWhile (`M.member` m) next
