module Day18 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, bounds, neighbours4)
import AOC.Data.Tuple (fromList2)
import AOC.Pathing.Dijkstra (dijkstra)
import Data.Bifunctor (Bifunctor (first))
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 18 (part1 1024) (part2 1024)

parse :: Int -> T.Text -> (CoordMap Char, [((Int, Int), Char)])
parse n =
  first
    ( ( \cm ->
          let (minx, maxx, miny, maxy) = bounds cm
           in foldl
                (\m (x, y) -> M.insertWith (\_ o -> o) (x, y) '.' m)
                cm
                [(x, y) | x <- [minx .. maxx], y <- [miny .. maxy]]
      )
        . M.fromList
    )
    . splitAt n
    . map ((,'#') . fromList2 . map (read . T.unpack) . T.splitOn ",")
    . T.lines

expand :: CoordMap Char -> Coord -> [(Coord, Int)]
expand cm =
  map ((,1) . fst)
    . filter ((== '.') . snd)
    . (`neighbours4` cm)

findPath :: CoordMap Char -> Maybe (Int, [[Coord]])
findPath cm =
  let (_, x, _, y) = bounds cm
   in dijkstra (expand cm) (0, 0) (== (x, y))

part1 :: Int -> T.Text -> Int
part1 n =
  fst
    . fromJust
    . findPath
    . fst
    . parse n

part2 :: Int -> T.Text -> String
part2 n =
  (\(x, y) -> show x ++ "," ++ show y)
    . snd
    . fromJust
    . find (isNothing . findPath . fst)
    . (\(cm, bs) -> scanl (\(m, _) (c, v) -> (M.insert c v m, c)) (cm, (0, 0)) bs)
    . parse n
