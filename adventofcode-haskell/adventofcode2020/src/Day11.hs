module Day11 (main, part1, part2) where

import Data.List (foldl', intercalate)
import qualified Data.Map as Map
import Data.Text (chunksOf, pack, unpack)

type SeatMap = Map.Map (Int, Int) Char

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

displayMap :: SeatMap -> String
displayMap seatMap = '\n' : intercalate "\n" (map unpack $ chunksOf (xMax + 1) (pack squares))
  where
    squares =
      [seatMap Map.! (x, y) | y <- [0 .. yMax], x <- [0 .. xMax]]
    xMax = maximum $ map fst coords
    yMax = maximum $ map snd coords
    coords = Map.keys seatMap

parseIntoCoordinateMap :: [String] -> SeatMap
parseIntoCoordinateMap =
  foldl'
    ( \m (y, line) ->
        Map.union
          m
          ( foldl'
              (\m' (x, char) -> Map.insert (x, y) char m')
              Map.empty
              $ zip [0 ..] line
          )
    )
    Map.empty
    . zip [0 ..]

-- Part 1

getAdjacents :: SeatMap -> (Int, Int) -> [Char]
getAdjacents seatMap (x, y) =
  map (seatMap Map.!) $
    filter
      (`elem` Map.keys seatMap)
      [(x', y') | x' <- [x -1, x, x + 1], y' <- [y -1, y, y + 1], (x', y') /= (x, y)]

getNewValue :: Char -> [Char] -> Char
getNewValue val adjacents = do
  let occupied = length $ filter (== '#') adjacents
  case val of
    'L' -> if occupied == 0 then '#' else val
    '#' -> if occupied >= 4 then 'L' else val
    _ -> val

applyRound :: SeatMap -> SeatMap
applyRound seatMap = do
  foldl' updateMap seatMap (Map.keys seatMap)
  where
    updateMap m (x, y) = do
      let val = seatMap Map.! (x, y)
      let adjacents = getAdjacents seatMap (x, y)
      Map.insert (x, y) (getNewValue val adjacents) m

findSettledState :: SeatMap -> SeatMap
findSettledState seatMap = do
  let seatMap' = applyRound seatMap
  if seatMap' == seatMap
    then seatMap
    else findSettledState seatMap'

-- Part 2

getVisibleInDirection :: SeatMap -> (Int, Int) -> (Int, Int) -> [Char]
getVisibleInDirection seatMap (x, y) (dx, dy) = do
  take 1 $ filter (/= '.') $ map (seatMap Map.!) $ takeWhile (`elem` Map.keys seatMap) [(x + dx * i, y + dy * i) | i <- [1 ..]]

getVisible :: SeatMap -> (Int, Int) -> [Char]
getVisible seatMap (x, y) =
  concatMap (getVisibleInDirection seatMap (x, y)) [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

getNewValue' :: Char -> [Char] -> Char
getNewValue' val adjacents = do
  let occupied = length $ filter (== '#') adjacents
  case val of
    'L' -> if occupied == 0 then '#' else val
    '#' -> if occupied >= 5 then 'L' else val
    _ -> val

applyRound' :: SeatMap -> SeatMap
applyRound' seatMap = do
  foldl' updateMap seatMap (Map.keys seatMap)
  where
    updateMap m (x, y) = do
      let val = seatMap Map.! (x, y)
      let adjacents = getVisible seatMap (x, y)
      Map.insert (x, y) (getNewValue' val adjacents) m

findSettledState' :: SeatMap -> SeatMap
findSettledState' seatMap = do
  let seatMap' = applyRound' seatMap
  if seatMap' == seatMap
    then seatMap
    else findSettledState' seatMap'

countOccupied :: SeatMap -> Int
countOccupied = length . filter (== '#') . Map.elems

part1 :: String -> String
part1 = show . countOccupied . findSettledState . parseIntoCoordinateMap . lines

part2 :: String -> String
part2 = show . countOccupied . findSettledState' . parseIntoCoordinateMap . lines
