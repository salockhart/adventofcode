module Day21 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, neighbours4, readCoordMap)
import Data.Bifunctor (second)
import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Text as T
import Debug.Trace (trace)
import Text.Printf (printf)

main :: IO ()
main = mkAoCMain 2023 21 (part1 64) (part2 26501365)

parse :: T.Text -> (CoordMap Char, Coord)
parse =
  (\m -> (m,) $ maximum $ M.keys m)
    . M.filter (/= '#')
    . readCoordMap
    . lines
    . T.unpack

solve garden startPosition = iterate solve' [startPosition]
  where
    solve' = nub . map fst . concatMap (`neighbours4` garden)

solve' n garden (maxX, maxY) = 0
  where
    (doubleTileSteps, extraSteps) = divMod n (maxX * 2)
    positions = (!! (maxX * 2 + extraSteps)) $ solve garden (maxX `div` 2, maxY `div` 2)

-- part1 :: p -> T.Text -> (CoordMap Int, CoordMap (Maybe Coord))
part1 n =
  length
    . (!! n)
    . uncurry solve
    . second startPosition
    . parse
  where
    startPosition (maxX, maxY) = (maxX `div` 2, maxY `div` 2)

-- inspired by https://work.njae.me.uk/2023/12/29/advent-of-code-2023-day-21/

part2 n =
  uncurry (solve' n)
    . parse
