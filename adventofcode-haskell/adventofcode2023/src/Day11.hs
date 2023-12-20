{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day11 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (CoordMap)
import Data.List (foldl', transpose)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 11 part1 (part2 1000000)

combinations2 :: [a] -> [[a]]
combinations2 [] = []
combinations2 (x : rest) = map (\y -> [x, y]) rest ++ combinations2 rest

distanceBetweenPoints :: [((Int, Int), b)] -> Int
distanceBetweenPoints [((xa, ya), _), ((xb, yb), _)] = abs (xb - xa) + abs (yb - ya)
distanceBetweenPoints _ = error "can't get the distance, doesn't match up"

parse :: Int -> [[Char]] -> [((Int, Int), Char)]
parse emptyLinePenalty ls = do
  let ys = getAxisPoints emptyLinePenalty ls
  let xs = getAxisPoints emptyLinePenalty (transpose ls)
  let m = readCoordMap' ys xs ls
  M.toList $ M.filter (/= '.') m

getAxisPoints :: Int -> [[Char]] -> [Int]
getAxisPoints emptyLinePenalty ls = numberLines' ls [0 ..]
  where
    numberLines' [] _ = []
    numberLines' (x : xs) (n : ns) =
      n
        : numberLines'
          xs
          (if not $ all (== '.') x then ns else drop (emptyLinePenalty - 1) ns)

readCoordMap' :: [Int] -> [Int] -> [[Char]] -> CoordMap Char
readCoordMap' ys xs =
  foldl'
    ( \m (y, line) ->
        M.union
          m
          ( foldl'
              (\m' (x, char) -> M.insert (x, y) char m')
              M.empty
              $ zip xs line
          )
    )
    M.empty
    . zip ys

solve :: Int -> T.Text -> Int
solve n =
  sum
    . map distanceBetweenPoints
    . combinations2
    . parse n
    . lines
    . T.unpack

part1 :: T.Text -> Int
part1 = solve 2

part2 :: Int -> T.Text -> Int
part2 = solve
