{-# LANGUAGE OverloadedStrings #-}

module Day13 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (first)
import Data.List (find, transpose)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 13 part1 part2

parse :: T.Text -> [[String]]
parse = map (lines . T.unpack) . T.splitOn "\n\n" . T.strip

solve :: Eq b => Int -> [[b]] -> Either Int Int
solve requiredDiffs ls =
  case solve' ls of
    -- if this worked, return it as "Left" i.e. "multiply me by 100"
    Just x -> Left x
    -- if not, transpose the lines and try again
    Nothing -> case solve' (transpose ls) of
      -- if this worked, return it as "Right" i.e. "don't multiply me"
      Just x -> Right x
      Nothing -> error "shouldn't be here"
  where
    solve' ls' = find (isMirrorLine ls') [1 .. length ls' - 1]
    isMirrorLine ls' =
      -- sum them up, and see if the number of mismatches
      -- matches our required number of mismatches
      -- 0 -> no difference, perfect mirror
      -- 1 -> one single difference, a "smudge"
      (== requiredDiffs)
        . sum
        -- map each pair to the number of mismatches present
        . map (\(a, b) -> length $ filter (uncurry (/=)) $ zip a b)
        -- reverse the first section and zip them together
        -- this way, we compare counterparts across the line
        . uncurry zip
        . first reverse
        -- split the lines at the attempted index
        . (`splitAt` ls')

part1 :: T.Text -> Int
part1 =
  sum
    . map (either (* 100) id . solve 0)
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map (either (* 100) id . solve 1)
    . parse
