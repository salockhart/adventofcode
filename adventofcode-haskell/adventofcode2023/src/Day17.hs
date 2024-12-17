module Day17 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (readCoordMap)
import AOC.Data.List (mapWithPrevious)
import AOC.Data.Tuple (fstOf3, sndOf3)
import AOC.Pathing.Dijkstra (dijkstra)
import Data.Bifunctor (Bifunctor (first), second)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 17 part1 part2

data Direction = Horizontal | Vertical
  deriving (Show, Ord)

instance Eq Direction where
  Horizontal == Horizontal = True
  Vertical == Vertical = True
  _ == _ = False

type CoordWithDirection = (Int, Int, Direction)

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
  M.Map CoordWithDirection Int ->
  CoordWithDirection ->
  [(CoordWithDirection, Int)]
neighbours minVal maxVal g (x, y, d) =
  map fst $
    concatMap
      (filter ((>= minVal) . snd))
      ( [ mapWithPrevious
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
      )

solve :: Int -> Int -> M.Map CoordWithDirection Int -> Int
solve minVal maxVal coordMap = do
  let coords = M.keys coordMap
  let sources = [(0, 0, Horizontal), (0, 0, Vertical)]
  let target (x, y, _) = (x, y) == (\cs -> (maximum $ map fstOf3 cs, maximum $ map sndOf3 cs)) coords
  minimum $ map fst $ mapMaybe (\s -> dijkstra (neighbours minVal maxVal coordMap) s target) sources

part1 :: T.Text -> Int
part1 = solve 1 3 . parse

part2 :: T.Text -> Int
part2 = solve 4 10 . parse
