module Day06Spec (spec) where

import Day06 (part1, part2)
import Test.Hspec

input = "3,4,3,1,2"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "5934"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "26984457539"
