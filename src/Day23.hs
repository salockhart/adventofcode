module Day23 (main, part1, part2) where

import Data.Char (digitToInt)
import qualified Data.IntMap as IntMap

type Cup = Int

type Cups = IntMap.IntMap Cup

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> (Cup, Cups)
parse str = do
  let cups = map digitToInt str
  (head cups, IntMap.fromList $ zip cups (tail (cycle cups)))

parse' :: String -> (Cup, Cups)
parse' str = do
  let cups = map digitToInt str
  let cups' = cups ++ [(maximum cups + 1) .. 1000000]
  (head cups', IntMap.fromList $ zip cups' (tail (cycle cups')))

findDestination :: Int -> Cup -> [Cup] -> Cup
findDestination maxLabel cup movedCups
  | cup `elem` movedCups = findDestination maxLabel (cup - 1) movedCups
  | cup < 1 = findDestination maxLabel maxLabel movedCups
  | otherwise = cup

connect :: Cup -> Cup -> Cups -> Cups
connect = IntMap.insert

play :: Int -> Int -> (Cup, Cups) -> (Cup, Cups)
play _ 0 x = x
play maxLabel round (curr, cups) = do
  let a = cups IntMap.! curr
  let b = cups IntMap.! a
  let c = cups IntMap.! b
  let next = cups IntMap.! c
  let dest = findDestination maxLabel (curr - 1) [a, b, c]
  let next' = cups IntMap.! dest
  let cups' =
        connect curr next $
          connect dest a $
            connect c next' cups
  play maxLabel (round - 1) (next, cups')

part1 :: String -> String
part1 =
  show
    . buildOrder [1]
    . play 9 100
    . parse
  where
    buildOrder order (curr, cups)
      | length order == 9 = sum $ zipWith (*) (map (10 ^) [0, 1 ..]) $ init order
      | otherwise = buildOrder ((cups IntMap.! head order) : order) (curr, cups)

part2 :: String -> String
part2 =
  show
    . getAnswer
    . play 1000000 10000000
    . parse'
  where
    getAnswer (_, cups) = do
      let a = cups IntMap.! 1
      let b = cups IntMap.! a
      a * b
