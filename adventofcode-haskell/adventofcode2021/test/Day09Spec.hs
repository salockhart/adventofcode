module Day09Spec (spec) where

import Day09 (part1, part2)
import Test.Hspec

input =
  "2199943210\n\
  \3987894921\n\
  \9856789892\n\
  \8767896789\n\
  \9899965678"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "15"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "1134"
