module Day15Spec (spec) where

import Day15 (part1, part2)
import Test.Hspec

input =
  "1163751742\n\
  \1381373672\n\
  \2136511328\n\
  \3694931569\n\
  \7463417111\n\
  \1319128137\n\
  \1359912421\n\
  \3125421639\n\
  \1293138521\n\
  \2311944581"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "40"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "315"
