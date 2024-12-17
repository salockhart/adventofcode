module Day16 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, readCoordMap)
import AOC.Pathing.Dijkstra (dijkstra)
import Data.List (find, nub)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Text as T

data Direction = N | E | S | W deriving (Show, Eq, Ord)

main :: IO ()
main = mkAoCMain 2024 16 part1 part2

parse :: T.Text -> M.Map (Coord, Direction) Char
parse =
  M.foldlWithKey (\m k v -> M.union m (M.fromList [((k, N), v), ((k, E), v), ((k, S), v), ((k, W), v)])) M.empty
    . readCoordMap
    . lines
    . T.unpack

neighbours :: (Coord, Direction) -> M.Map (Coord, Direction) Char -> [((Coord, Direction), Int)]
neighbours p m = catMaybes $ ahead p m : map pure (turns p m)

ahead :: (Coord, Direction) -> M.Map (Coord, Direction) Char -> Maybe ((Coord, Direction), Int)
ahead ((x, y), d) m =
  ahead' >>= (\(c', v) -> if v == '#' then Nothing else Just ((c', d), 1))
  where
    ahead' = case d of
      N -> let c' = (x, y - 1) in (c',) <$> m M.!? (c', N)
      E -> let c' = (x + 1, y) in (c',) <$> m M.!? (c', E)
      S -> let c' = (x, y + 1) in (c',) <$> m M.!? (c', S)
      W -> let c' = (x - 1, y) in (c',) <$> m M.!? (c', W)

turns :: (Coord, Direction) -> M.Map (Coord, Direction) Char -> [((Coord, Direction), Int)]
turns (c, d) m = filter (isJust . (`ahead` m) . fst) $ map (\d' -> ((c, d'), 1000)) turns'
  where
    turns'
      | d == N || d == S = [E, W]
      | otherwise = [N, S]

solve :: M.Map (Coord, Direction) Char -> (Int, [[(Coord, Direction)]])
solve m =
  let start = fst $ fromJust $ find (\((_, d), v) -> (d, v) == (E, 'S')) (M.toList m)
   in fromJust $ dijkstra (`neighbours` m) start (\c -> m M.!? c == Just 'E')

part1 :: T.Text -> Int
part1 =
  fst
    . solve
    . parse

part2 :: T.Text -> Int
part2 =
  length
    . nub
    . map fst
    . concat
    . snd
    . solve
    . parse
