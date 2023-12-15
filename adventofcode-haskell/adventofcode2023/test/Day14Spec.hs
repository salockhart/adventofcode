{-# LANGUAGE OverloadedStrings #-}

module Day14Spec (spec) where

import qualified Data.Text as T
import Day14 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "O....#....\n\
  \O.OO#....#\n\
  \.....##...\n\
  \OO.#O....O\n\
  \.O.....O#.\n\
  \O.#..O.#.#\n\
  \..O..#O..O\n\
  \.......O..\n\
  \#....###..\n\
  \#OO..#...."

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 136)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 64)
