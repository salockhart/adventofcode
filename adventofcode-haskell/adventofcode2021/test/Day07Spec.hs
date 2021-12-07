module Day07Spec (spec) where

import Day07 (part1, part2)
import Test.Hspec

input = "16,1,2,0,4,2,7,1,2,14"

spec :: Spec
spec = do
  describe "Part1" $ do
    it input $ do
      part1 input `shouldBe` "37"

  describe "Part2" $ do
    it input $ do
      part2 input `shouldBe` "168"
