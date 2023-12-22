{-# LANGUAGE OverloadedStrings #-}

module Day17Spec (spec) where

import qualified Data.Text as T
import Day17 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "2413432311323\n\
  \3215453535623\n\
  \3255245654254\n\
  \3446585845452\n\
  \4546657867536\n\
  \1438598798454\n\
  \4457876987766\n\
  \3637877979653\n\
  \4654967986887\n\
  \4564679986453\n\
  \1224686865563\n\
  \2546548887735\n\
  \4322674655533"

input2 :: T.Text
input2 =
  "111111111111\n\
  \999999999991\n\
  \999999999991\n\
  \999999999991\n\
  \999999999991"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 102)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input1) $ do
      part2 input1 `shouldSatisfy` (== 94)
    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 71)
