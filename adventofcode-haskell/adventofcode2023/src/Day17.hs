module Day17 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (readCoordMap)
import AOC.Data.Tuple (fstOf3, sndOf3)
import AOC.Pathing.Dijkstra (dijkstra)
import Data.Bifunctor (Bifunctor (first), second)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T

data Direction = Horizontal | Vertical
  deriving (Show, Ord)

instance Eq Direction where
  Horizontal == Horizontal = True
  Vertical == Vertical = True
  _ == _ = False

type CoordWithDirection = (Int, Int, Direction)

main :: IO ()
main = mkAoCMain 2023 17 part1 part2

mapWithPrevious :: (a -> a -> a) -> [a] -> [a]
mapWithPrevious _ [] = []
mapWithPrevious fn xs = head xs : mapWithPrevious' xs
  where
    mapWithPrevious' [] = []
    mapWithPrevious' [_] = []
    mapWithPrevious' (a : b : rest) = let b' = fn a b in b' : mapWithPrevious' (b' : rest)

parse :: T.Text -> M.Map CoordWithDirection Int
parse =
  M.foldlWithKey (\m (x, y) v -> m `M.union` M.fromList [((x, y, z), v) | z <- [Horizontal, Vertical]]) M.empty
    . readCoordMap
    . map (map (\x -> read [x] :: Int))
    . lines
    . T.unpack

neighbours ::
  Int ->
  Int ->
  CoordWithDirection ->
  M.Map CoordWithDirection Int ->
  [(CoordWithDirection, Int)]
neighbours minVal maxVal (x, y, d) g =
  map fst $
    filter ((>= minVal) . snd) $
      concat
        [ mapWithPrevious
            (\(prev, _) current -> first (second (+ snd prev)) current)
            [ ((n, cost), i)
              | i <- [1 .. maxVal],
                let n =
                      if d == Horizontal
                        then (x + (i * s), y, Vertical)
                        else (x, y + (i * s), Horizontal),
                n `M.member` g,
                let cost = g M.! n
            ]
          | s <- [-1, 1]
        ]

solve :: Int -> Int -> M.Map CoordWithDirection Int -> Int
solve minVal maxVal coordMap = do
  let coords = M.keys coordMap
  let sources = [(0, 0, Horizontal), (0, 0, Vertical)]
  let target (x, y, _) = (x, y) == (\cs -> (maximum $ map fstOf3 cs, maximum $ map sndOf3 cs)) coords
  let (cost, _) = dijkstra (neighbours minVal maxVal) sources target coordMap
  fromJust cost

part1 :: T.Text -> Int
part1 = solve 1 3 . parse

part2 :: T.Text -> Int
part2 = solve 4 10 . parse
