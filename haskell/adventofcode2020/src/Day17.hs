module Day17 (main, part1, part2) where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)

type Coord = (Int, Int, Int)

type Coord' = (Int, Int, Int, Int)

type SourceMap = Map.Map Coord Char

type SourceMap' = Map.Map Coord' Char

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [[Char]] -> SourceMap
parse =
  foldl'
    ( \map (y, row) ->
        foldl'
          (\map' (x, char) -> Map.insert (x, y, 0) char map')
          map
          row
    )
    Map.empty
    . zip [0 ..]
    . map (zip [0 ..])

bounds :: SourceMap -> (Coord, Coord)
bounds source = do
  let keys = Map.keys source
  let (xs, ys, zs) = foldl' (\(xs, ys, zs) (x, y, z) -> (x : xs, y : ys, z : zs)) ([], [], []) keys
  ((minimum xs - 1, minimum ys - 1, minimum zs - 1), (maximum xs + 1, maximum ys + 1, maximum zs + 1))

execute :: SourceMap -> SourceMap
execute source = do
  let ((lx, ly, lz), (ux, uy, uz)) = bounds source
  let coords = [(x, y, z) | x <- [lx .. ux], y <- [ly .. uy], z <- [lz .. uz]]
  foldl' apply source coords
  where
    apply source' coord = do
      let me = fromMaybe '.' $ source Map.!? coord
      let adjs = mapMaybe (source Map.!?) $ adjacents coord
      let numActive = length $ filter (== '#') adjs
      case me of
        '#' -> if numActive `elem` [2, 3] then source' else Map.insert coord '.' source'
        '.' -> if numActive == 3 then Map.insert coord '#' source' else source'
    adjacents (x, y, z) =
      [(x', y', z') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1], z' <- [z - 1, z, z + 1], (x', y', z') /= (x, y, z)]

parse' :: [[Char]] -> SourceMap'
parse' =
  foldl'
    ( \map (y, row) ->
        foldl'
          (\map' (x, char) -> Map.insert (x, y, 0, 0) char map')
          map
          row
    )
    Map.empty
    . zip [0 ..]
    . map (zip [0 ..])

bounds' :: SourceMap' -> (Coord', Coord')
bounds' source = do
  let keys = Map.keys source
  let (xs, ys, zs, ws) = foldl' (\(xs, ys, zs, ws) (x, y, z, w) -> (x : xs, y : ys, z : zs, w : ws)) ([], [], [], []) keys
  ((minimum xs - 1, minimum ys - 1, minimum zs - 1, minimum ws - 1), (maximum xs + 1, maximum ys + 1, maximum zs + 1, maximum ws + 1))

execute' :: SourceMap' -> SourceMap'
execute' source = do
  let ((lx, ly, lz, lw), (ux, uy, uz, uw)) = bounds' source
  let coords = [(x, y, z, w) | x <- [lx .. ux], y <- [ly .. uy], z <- [lz .. uz], w <- [lw .. uw]]
  foldl' apply source coords
  where
    apply source' coord = do
      let me = fromMaybe '.' $ source Map.!? coord
      let adjs = mapMaybe (source Map.!?) $ adjacents coord
      let numActive = length $ filter (== '#') adjs
      case me of
        '#' -> if numActive `elem` [2, 3] then source' else Map.insert coord '.' source'
        '.' -> if numActive == 3 then Map.insert coord '#' source' else source'
    adjacents (x, y, z, w) =
      [(x', y', z', w') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1], z' <- [z - 1, z, z + 1], w' <- [w - 1, w, w + 1], (x', y', z', w') /= (x, y, z, w)]

runCycles :: (a -> a) -> a -> a
runCycles exe source = foldl' (\m _ -> exe m) source [1 .. 6]

countActive :: Map.Map a Char -> Int
countActive = length . filter (== '#') . Map.elems

part1 :: String -> String
part1 = show . countActive . runCycles execute . parse . lines

part2 :: String -> String
part2 = show . countActive . runCycles execute' . parse' . lines
