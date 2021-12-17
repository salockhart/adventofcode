module Day17Spec (spec) where

import Day17 (part1, part2)
import Test.Hspec

input = "target area: x=20..30, y=-10..-5"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "45"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "112"
