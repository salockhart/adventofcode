{-# LANGUAGE OverloadedStrings #-}

module Day13Spec (spec) where

import qualified Data.Text as T
import Day13 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "#.##..##.\n\
  \..#.##.#.\n\
  \##......#\n\
  \##......#\n\
  \..#.##.#.\n\
  \..##..##.\n\
  \#.#.##.#.\n\
  \\n\
  \#...##..#\n\
  \#....#..#\n\
  \..##..###\n\
  \#####.##.\n\
  \#####.##.\n\
  \..##..###\n\
  \#....#..#"

input2 :: T.Text
input2 =
  "###...#...#.#..\n\
  \.#.##.#.....#..\n\
  \..###..#..#....\n\
  \..###..#..#....\n\
  \.#.##.#........\n\
  \###...#...#.#..\n\
  \###..#..##.#.##"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 405)
    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 14)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input1) $ do
      part2 input1 `shouldSatisfy` (== 400)
