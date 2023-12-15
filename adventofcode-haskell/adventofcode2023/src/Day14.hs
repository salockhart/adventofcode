module Day14 (main, part1, part2) where

import AOC (solveAoCDay)
import Data.List (elemIndex, find, groupBy, inits, sort, transpose)
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = solveAoCDay 2023 14 part1 part2

mountainTilt :: [Char] -> [Char]
mountainTilt = concatMap (reverse . sort) . groupBy groupStones
  where
    groupStones '#' '#' = True
    groupStones 'O' 'O' = True
    groupStones '.' '.' = True
    groupStones '.' 'O' = True
    groupStones 'O' '.' = True
    groupStones _ _ = False

mountainCycle :: [[Char]] -> [[Char]]
mountainCycle = (!! 4) . iterate (rotateCW . map mountainTilt)
  where
    rotateCW = reverse . transpose

solveMountainLoad :: [[Char]] -> Int
solveMountainLoad =
  sum
    . map
      ( sum
          . map fst
          . filter ((== 'O') . snd)
          . zip [1 ..]
          . reverse
      )

part1 :: T.Text -> Int
part1 =
  solveMountainLoad
    . map mountainTilt
    . transpose
    . lines
    . T.unpack

part2 :: T.Text -> Int
part2 =
  solveMountainLoad
    -- then fetch the required result
    . (\f -> f 1000000000)
    -- using our knowledge of the repeat, build an equation for fetching any cycle result
    . ( \ms ->
          let repeatIndex = fromJust (elemIndex (last ms) (init ms)) + 1
           in (\n -> ms !! (((n - repeatIndex) `mod` (length ms - repeatIndex)) + repeatIndex))
      )
    . fromJust
    -- and find the first one where we have a repeat (last aka latest is in the list)
    . find (\mss -> length mss > 1 && last mss `elem` init mss)
    -- slowly build up lists of more and more cycles
    . inits
    . iterate mountainCycle
    . transpose
    . lines
    . T.unpack
