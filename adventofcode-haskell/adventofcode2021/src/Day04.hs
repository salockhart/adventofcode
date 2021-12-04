module Day04 (main, part1, part2) where

import AOC (slice, splitOn)
import Data.Bits (Bits (xor))
import Data.List (elemIndex, findIndex, transpose)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Debug.Trace (trace)

type Row = [Int]

type Board = [Row]

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: String -> ([Int], [Board])
parse input = do
  let ns : bs = splitOn "\n\n" input
  let ns' = map read $ splitOn "," ns
  let bs' = map parseBoard bs
  (ns', bs')
  where
    parseBoard = map (map read . words) . lines

winningLines = map (\b -> b ++ transpose b)

forceUnwrap (Just x) = x
forceUnwrap Nothing = error "cannot unwrap"

solve sf (ns, bs) = score winningBoard
  where
    timeForRowToWin = forceUnwrap . maximum . map (`elemIndex` ns)
    timeForBoardToWin = minimum . map timeForRowToWin
    winningBoard = do
      let boardWinTimes = map timeForBoardToWin $ winningLines bs
      let winningBoard = (\ts -> forceUnwrap $ elemIndex (sf ts) ts) boardWinTimes
      (winningBoard, boardWinTimes !! winningBoard)
    score (wi, wt) = do
      let drawnNumbers = slice 0 wt ns
      let board = bs !! wi
      let unmarked = filter (\e -> isNothing (elemIndex e drawnNumbers)) $ concat board
      sum unmarked * last drawnNumbers

part1 :: String -> String
part1 = show . solve minimum . parse

part2 :: String -> String
part2 = show . solve maximum . parse
