module Day11 (main, part1, part2) where

import AOC
  ( Coord,
    CoordMap,
    applyN,
    getDiagonals,
    getNeighbours,
    parseIntoCoordMap,
    traceCoordMap,
  )
import Data.List (nub, sort)
import qualified Data.Map as Map
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> CoordMap Int
parse = parseIntoCoordMap . map (map (\c -> read [c])) . lines

incrementCoords :: CoordMap Int -> [Coord] -> CoordMap Int
incrementCoords m cs =
  Map.mapWithKey
    ( \k f ->
        let numOccurences = length $ filter (== k) cs
         in if f == 0 then 0 else f + numOccurences
    )
    m

getAllNeighbours :: CoordMap Int -> [Coord] -> [Coord]
getAllNeighbours m = concatMap (Map.keys . getRing) . nub
  where
    getRing c = getNeighbours m c `Map.union` getDiagonals m c

handleFlashed :: CoordMap Int -> (CoordMap Int, [Coord])
handleFlashed m =
  Map.foldlWithKey'
    ( \(m', cs) c f ->
        if f > 9
          then (Map.update (const (Just 0)) c m', c : cs)
          else (m', cs)
    )
    (m, [])
    m

applyStep :: CoordMap Int -> CoordMap Int
applyStep om = do
  let om' = Map.map succ om
  fst $
    head $
      dropWhile (\(_, cs) -> not (null cs)) $
        drop 1 $
          iterate
            ( \(m, cs) -> do
                let flashed = getAllNeighbours m cs
                let incremented = incrementCoords m flashed
                let (a, b) = handleFlashed incremented
                (a, b)
            )
            (om', [])

part1 :: String -> String
part1 =
  show
    . foldl (\x y -> x + length (Map.filter (== 0) y)) 0
    . take 100
    . drop 1
    . iterate applyStep
    . parse

part2 :: String -> String
part2 =
  show
    . length
    . takeWhile (not . (all (== 0) . Map.elems))
    . iterate applyStep
    . parse
