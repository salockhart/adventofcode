module Day21 (main, part1, part2) where

import AOC (dbg)
import Debug.Trace (trace)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> (Int, Int)
parse = (\xs -> (xs !! 0, xs !! 1)) . map (read . drop 28) . lines

play = play' 0 (cycle [1 .. 100]) (0, 0)
  where
    play' n (rollA1 : rollA2 : rollA3 : rollB1 : rollB2 : rollB3 : rolls) (scoreA, scoreB) (posA, posB) = do
      let posA' = ((posA - 1 + rollA1 + rollA2 + rollA3) `mod` 10) + 1
      let scoreA' = scoreA + posA'
      let posB' = ((posB - 1 + rollB1 + rollB2 + rollB3) `mod` 10) + 1
      let scoreB' = scoreB + posB'
      if scoreA' >= 1000
        then scoreB * (n + 3)
        else
          if scoreB' >= 1000
            then scoreA' * (n + 6)
            else play' (n + 6) rolls (scoreA', scoreB') (posA', posB')
    play' _ _ _ _ = error "cannot play"

part1 :: String -> String
part1 =
  show
    . play
    . parse

part2 :: String -> String
part2 = show . const 1
